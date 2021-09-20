{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Functions for working with comment stream.
module Ormolu.Parser.CommentStream
  ( -- * Comment stream
    CommentStream (..),
    mkCommentStream,
    showCommentStream,

    -- * Comment
    Comment (..),
    unComment,
    hasAtomsBefore,
    isMultilineComment,
  )
where

import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.Data (Data)
import Data.Generics.Schemes
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Set as S
import GHC.Hs (HsModule)
import GHC.Hs.Decls (HsDecl (..), LDocDecl, LHsDecl)
import GHC.Hs.Doc
import GHC.Hs.Extension
import GHC.Hs.ImpExp
import GHC.Parser.Annotation (EpAnnComments (..), getLocA)
import qualified GHC.Parser.Annotation as GHC
import GHC.Types.SrcLoc
import Ormolu.Parser.Pragma
import Ormolu.Utils (onTheSameLine, showOutputable)

----------------------------------------------------------------------------
-- Comment stream

-- | A stream of 'RealLocated' 'Comment's in ascending order with respect to
-- beginning of corresponding spans.
newtype CommentStream = CommentStream [RealLocated Comment]
  deriving (Eq, Data, Semigroup, Monoid)

-- | Create 'CommentStream' from 'HsModule'. The pragmas are
-- removed from the 'CommentStream'.
mkCommentStream ::
  -- | Original input
  String ->
  -- | Module to use for comment extraction
  HsModule ->
  -- | Stack header, pragmas, and comment stream
  ( Maybe (RealLocated Comment),
    [([RealLocated Comment], Pragma)],
    CommentStream
  )
mkCommentStream input hsModule =
  ( mstackHeader,
    pragmas,
    CommentStream comments
  )
  where
    (comments, pragmas) = extractPragmas input rawComments1
    (rawComments1, mstackHeader) = extractStackHeader rawComments0

    -- We want to extract all comments except _valid_ Haddock comments
    rawComments0 =
      fmap (uncurry L)
        . M.toAscList
        . flip M.withoutKeys validHaddockCommentSpans
        . M.fromList
        . fmap (\(L l a) -> (l, a))
        $ allComments
      where
        -- All comments, including valid and invalid Haddock comments
        allComments =
          mapMaybe unAnnotationComment $
            epAnnCommentsToList =<< listify (only @EpAnnComments) hsModule
          where
            epAnnCommentsToList = \case
              EpaComments cs -> cs
              EpaCommentsBalanced pcs fcs -> pcs <> fcs
        -- All spans of valid Haddock comments
        -- (everywhere where we use p_hsDoc{String,Name})
        validHaddockCommentSpans =
          S.fromList
            . mapMaybe srcSpanToRealSrcSpan
            . mconcat
              [ fmap getLoc . listify (only @LHsDocString),
                fmap getLocA . listify (only @(LDocDecl GhcPs)),
                fmap getLocA . listify isDocD,
                fmap getLocA . listify isIEDocLike
              ]
            $ hsModule
          where
            isDocD :: LHsDecl GhcPs -> Bool
            isDocD = \case
              L _ DocD {} -> True
              _ -> False
            isIEDocLike :: LIE GhcPs -> Bool
            isIEDocLike = \case
              L _ IEGroup {} -> True
              L _ IEDoc {} -> True
              L _ IEDocNamed {} -> True
              _ -> False
    only :: a -> Bool
    only _ = True

-- | Pretty-print a 'CommentStream'.
showCommentStream :: CommentStream -> String
showCommentStream (CommentStream xs) =
  unlines $
    showComment <$> xs
  where
    showComment (L l str) = showOutputable l ++ " " ++ show str

----------------------------------------------------------------------------
-- Comment

-- | A wrapper for a single comment. The 'Bool' indicates whether there were
-- atoms before beginning of the comment in the original input. The
-- 'NonEmpty' list inside contains lines of multiline comment @{- … -}@ or
-- just single item\/line otherwise.
data Comment = Comment Bool (NonEmpty String)
  deriving (Eq, Show, Data)

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.
mkComment ::
  -- | Lines of original input with their indices
  [(Int, String)] ->
  -- | Raw comment string
  RealLocated String ->
  -- | Remaining lines of original input and the constructed 'Comment'
  ([(Int, String)], RealLocated Comment)
mkComment ls (L l s) = (ls', comment)
  where
    comment =
      L l . Comment atomsBefore . removeConseqBlanks . fmap dropTrailing $
        case NE.nonEmpty (lines s) of
          Nothing -> s :| []
          Just (x :| xs) ->
            let getIndent y =
                  if all isSpace y
                    then startIndent
                    else length (takeWhile isSpace y)
                n = minimum (startIndent : fmap getIndent xs)
                commentPrefix = if "{-" `L.isPrefixOf` s then "" else "-- "
             in x :| ((commentPrefix <>) . drop n <$> xs)
    (atomsBefore, ls') =
      case dropWhile ((< commentLine) . fst) ls of
        [] -> (False, [])
        ((_, i) : ls'') ->
          case take 2 (dropWhile isSpace i) of
            "--" -> (False, ls'')
            "{-" -> (False, ls'')
            _ -> (True, ls'')
    dropTrailing = L.dropWhileEnd isSpace
    startIndent = srcSpanStartCol l - 1
    commentLine = srcSpanStartLine l

-- | Get a collection of lines from a 'Comment'.
unComment :: Comment -> NonEmpty String
unComment (Comment _ xs) = xs

-- | Check whether the 'Comment' had some non-whitespace atoms in front of
-- it in the original input.
hasAtomsBefore :: Comment -> Bool
hasAtomsBefore (Comment atomsBefore _) = atomsBefore

-- | Is this comment multiline-style?
isMultilineComment :: Comment -> Bool
isMultilineComment (Comment _ (x :| _)) = "{-" `L.isPrefixOf` x

----------------------------------------------------------------------------
-- Helpers

-- | Detect and extract stack header if it is present.
extractStackHeader ::
  -- | Comment stream to analyze
  [RealLocated String] ->
  ([RealLocated String], Maybe (RealLocated Comment))
extractStackHeader = \case
  [] -> ([], Nothing)
  (x : xs) ->
    let comment = snd (mkComment [] x)
     in if isStackHeader (unRealSrcSpan comment)
          then (xs, Just comment)
          else (x : xs, Nothing)
  where
    isStackHeader (Comment _ (x :| _)) =
      "stack" `L.isPrefixOf` dropWhile isSpace (drop 2 x)

-- | Extract pragmas and their associated comments.
extractPragmas ::
  -- | Input
  String ->
  -- | Comment stream to analyze
  [RealLocated String] ->
  ([RealLocated Comment], [([RealLocated Comment], Pragma)])
extractPragmas input = go initialLs id id
  where
    initialLs = zip [1 ..] (lines input)
    go ls csSoFar pragmasSoFar = \case
      [] -> (csSoFar [], pragmasSoFar [])
      (x : xs) ->
        case parsePragma (unRealSrcSpan x) of
          Nothing ->
            let (ls', x') = mkComment ls x
             in go ls' (csSoFar . (x' :)) pragmasSoFar xs
          Just pragma ->
            let combined ys = (csSoFar ys, pragma)
                go' ls' ys rest = go ls' id (pragmasSoFar . (combined ys :)) rest
             in case xs of
                  [] -> go' ls [] xs
                  (y : ys) ->
                    let (ls', y') = mkComment ls y
                     in if onTheSameLine
                          (RealSrcSpan (getRealSrcSpan x) Nothing)
                          (RealSrcSpan (getRealSrcSpan y) Nothing)
                          then go' ls' [y'] ys
                          else go' ls [] xs

-- | Extract @'RealLocated' 'String'@ from 'GHC.LEpaComment'.
unAnnotationComment :: GHC.LEpaComment -> Maybe (RealLocated String)
unAnnotationComment (L (GHC.Anchor anchor _) (GHC.EpaComment eck _)) = case eck of
  GHC.EpaDocCommentNext s -> haddock s -- @-- |@
  GHC.EpaDocCommentPrev s -> haddock s -- @-- ^@
  GHC.EpaDocCommentNamed s -> haddock s -- @-- $@
  GHC.EpaDocSection _ s -> haddock s -- @-- *@
  GHC.EpaDocOptions s -> mkL s
  GHC.EpaLineComment s -> mkL $
    case take 3 s of
      "-- " -> s
      "---" -> s
      _ -> let s' = insertAt " " s 3 in s'
  GHC.EpaBlockComment s -> mkL s
  GHC.EpaEofComment -> Nothing
  where
    mkL = Just . L anchor
    insertAt x xs n = take (n - 1) xs ++ x ++ drop (n - 1) xs
    haddock = mkL . dashPrefix <=< dropBlank
      where
        dashPrefix s = "--" <> spaceIfNecessary <> s
          where
            spaceIfNecessary = case s of
              c : _ | c /= ' ' -> " "
              _ -> ""
        dropBlank :: String -> Maybe String
        dropBlank s = if all isSpace s then Nothing else Just s

-- | Remove consecutive blank lines.
removeConseqBlanks :: NonEmpty String -> NonEmpty String
removeConseqBlanks (x :| xs) = x :| go (null x) id xs
  where
    go seenBlank acc = \case
      [] -> acc []
      (y : ys) ->
        if seenBlank && null y
          then go True acc ys
          else go (null y) (acc . (y :)) ys

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.Strict as Strict
import GHC.Hs (HsModule)
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
  Text ->
  -- | Module to use for comment extraction
  HsModule GhcPs ->
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
        validHaddockCommentSpans =
          S.fromList
            . mapMaybe srcSpanToRealSrcSpan
            . mconcat
              [ fmap getLoc . listify (only @(LHsDoc GhcPs)),
                fmap getLocA . listify isIEDocLike
              ]
            $ hsModule
          where
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
data Comment = Comment Bool (NonEmpty Text)
  deriving (Eq, Show, Data)

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.
mkComment ::
  -- | Lines of original input with their indices
  [(Int, Text)] ->
  -- | Raw comment string
  RealLocated Text ->
  -- | Remaining lines of original input and the constructed 'Comment'
  ([(Int, Text)], RealLocated Comment)
mkComment ls (L l s) = (ls', comment)
  where
    comment =
      L l . Comment atomsBefore . removeConseqBlanks . fmap T.stripEnd $
        case NE.nonEmpty (T.lines s) of
          Nothing -> s :| []
          Just (x :| xs) ->
            let getIndent y =
                  if T.all isSpace y
                    then startIndent
                    else T.length (T.takeWhile isSpace y)
                n = minimum (startIndent : fmap getIndent xs)
                commentPrefix = if "{-" `T.isPrefixOf` s then "" else "-- "
             in x :| ((commentPrefix <>) . escapeHaddockTriggers . T.drop n <$> xs)
    (atomsBefore, ls') =
      case dropWhile ((< commentLine) . fst) ls of
        [] -> (False, [])
        ((_, i) : ls'') ->
          case T.take 2 (T.stripStart i) of
            "--" -> (False, ls'')
            "{-" -> (False, ls'')
            _ -> (True, ls'')
    startIndent
      -- srcSpanStartCol counts columns starting from 1, so we subtract 1
      | "{-" `T.isPrefixOf` s = srcSpanStartCol l - 1
      -- For single-line comments, the only case where xs != [] is when an
      -- invalid haddock comment composed of several single-line comments is
      -- encountered. In that case, each line of xs is prefixed with an
      -- extra space (not present in the original comment), so we set
      -- startIndent = 1 to remove this space.
      | otherwise = 1
    commentLine = srcSpanStartLine l

-- | Get a collection of lines from a 'Comment'.
unComment :: Comment -> NonEmpty Text
unComment (Comment _ xs) = xs

-- | Check whether the 'Comment' had some non-whitespace atoms in front of
-- it in the original input.
hasAtomsBefore :: Comment -> Bool
hasAtomsBefore (Comment atomsBefore _) = atomsBefore

-- | Is this comment multiline-style?
isMultilineComment :: Comment -> Bool
isMultilineComment (Comment _ (x :| _)) = "{-" `T.isPrefixOf` x

----------------------------------------------------------------------------
-- Helpers

-- | Detect and extract stack header if it is present.
extractStackHeader ::
  -- | Comment stream to analyze
  [RealLocated Text] ->
  ([RealLocated Text], Maybe (RealLocated Comment))
extractStackHeader = \case
  [] -> ([], Nothing)
  (x : xs) ->
    let comment = snd (mkComment [] x)
     in if isStackHeader (unRealSrcSpan comment)
          then (xs, Just comment)
          else (x : xs, Nothing)
  where
    isStackHeader (Comment _ (x :| _)) =
      "stack" `T.isPrefixOf` T.stripStart (T.drop 2 x)

-- | Extract pragmas and their associated comments.
extractPragmas ::
  -- | Input
  Text ->
  -- | Comment stream to analyze
  [RealLocated Text] ->
  ([RealLocated Comment], [([RealLocated Comment], Pragma)])
extractPragmas input = go initialLs id id
  where
    initialLs = zip [1 ..] (T.lines input)
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
                          (RealSrcSpan (getRealSrcSpan x) Strict.Nothing)
                          (RealSrcSpan (getRealSrcSpan y) Strict.Nothing)
                          then go' ls' [y'] ys
                          else go' ls [] xs

-- | Extract @'RealLocated' 'Text'@ from 'GHC.LEpaComment'.
unAnnotationComment :: GHC.LEpaComment -> Maybe (RealLocated Text)
unAnnotationComment (L (GHC.Anchor anchor _) (GHC.EpaComment eck _)) =
  case eck of
    GHC.EpaDocComment s ->
      let trigger = case s of
            MultiLineDocString t _ -> Just t
            NestedDocString t _ -> Just t
            -- should not occur
            GeneratedDocString _ -> Nothing
       in haddock trigger (T.pack $ renderHsDocString s)
    GHC.EpaDocOptions s -> mkL (T.pack s)
    GHC.EpaLineComment (T.pack -> s) -> mkL $
      case T.take 3 s of
        "-- " -> s
        "---" -> s
        _ -> insertAt " " s 3
    GHC.EpaBlockComment s -> mkL (T.pack s)
    GHC.EpaEofComment -> Nothing
  where
    mkL = Just . L anchor
    insertAt x xs n = T.take (n - 1) xs <> x <> T.drop (n - 1) xs
    haddock mtrigger =
      mkL . dashPrefix . escapeHaddockTriggers . (trigger <>) <=< dropBlank
      where
        trigger = case mtrigger of
          Just HsDocStringNext -> "|"
          Just HsDocStringPrevious -> "^"
          Just (HsDocStringNamed n) -> "$" <> T.pack n
          Just (HsDocStringGroup k) -> T.replicate k "*"
          Nothing -> ""
        dashPrefix s = "--" <> spaceIfNecessary <> s
          where
            spaceIfNecessary = case T.uncons s of
              Just (c, _) | c /= ' ' -> " "
              _ -> ""
        dropBlank :: Text -> Maybe Text
        dropBlank s = if T.all isSpace s then Nothing else Just s

-- | Remove consecutive blank lines.
removeConseqBlanks :: NonEmpty Text -> NonEmpty Text
removeConseqBlanks (x :| xs) = x :| go (T.null x) id xs
  where
    go seenBlank acc = \case
      [] -> acc []
      (y : ys) ->
        if seenBlank && T.null y
          then go True acc ys
          else go (T.null y) (acc . (y :)) ys

-- | Escape characters that can turn a line into a Haddock.
escapeHaddockTriggers :: Text -> Text
escapeHaddockTriggers string
  | Just (h, _) <- T.uncons string, h `elem` ("|^*$" :: [Char]) = T.cons '\\' string
  | otherwise = string

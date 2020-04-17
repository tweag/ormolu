{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Functions for working with comment stream.
module Ormolu.Parser.CommentStream
  ( CommentStream (..),
    Comment (..),
    mkCommentStream,
    isPrevHaddock,
    isMultilineComment,
    showCommentStream,
  )
where

import Data.Char (isSpace)
import Data.Data (Data)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified GHC
import qualified Lexer as GHC
import Ormolu.Parser.Pragma
import Ormolu.Parser.Shebang
import Ormolu.Utils (showOutputable)
import SrcLoc

-- | A stream of 'RealLocated' 'Comment's in ascending order with respect to
-- beginning of corresponding spans.
newtype CommentStream = CommentStream [RealLocated Comment]
  deriving (Eq, Data, Semigroup, Monoid)

-- | A wrapper for a single comment. The 'NonEmpty' list inside contains
-- lines of multiline comment @{- … -}@ or just single item\/line otherwise.
newtype Comment = Comment (NonEmpty String)
  deriving (Eq, Show, Data)

-- | Create 'CommentStream' from 'GHC.PState'. The pragmas and shebangs are
-- removed from the 'CommentStream'. Shebangs are only extracted from the
-- comments that come from the first argument.
mkCommentStream ::
  -- | Extra comments to include
  [Located String] ->
  -- | Parser state to use for comment extraction
  GHC.PState ->
  -- | Stack header, shebangs, pragmas, and comment stream
  ( Maybe (RealLocated Comment),
    [Shebang],
    [([RealLocated Comment], Pragma)],
    CommentStream
  )
mkCommentStream extraComments pstate =
  ( mstackHeader,
    shebangs,
    pragmas,
    CommentStream comments
  )
  where
    (comments, pragmas) = extractPragmas rawComments1
    (rawComments1, mstackHeader) = extractStackHeader rawComments0
    rawComments0 =
      L.sortOn (realSrcSpanStart . getRealSrcSpan) . mapMaybe toRealSpan $
        otherExtraComments
          ++ mapMaybe (liftMaybe . fmap unAnnotationComment) (GHC.comment_q pstate)
          ++ concatMap
            (mapMaybe (liftMaybe . fmap unAnnotationComment) . snd)
            (GHC.annotations_comments pstate)
    (shebangs, otherExtraComments) = extractShebangs extraComments

-- | Test whether a 'Comment' looks like a Haddock following a definition,
-- i.e. something starting with @-- ^@.
isPrevHaddock :: Comment -> Bool
isPrevHaddock (Comment (x :| _)) = "-- ^" `L.isPrefixOf` x

-- | Is this comment multiline-style?
isMultilineComment :: Comment -> Bool
isMultilineComment (Comment (x :| _)) = "{-" `L.isPrefixOf` x

-- | Pretty-print a 'CommentStream'.
showCommentStream :: CommentStream -> String
showCommentStream (CommentStream xs) =
  unlines $
    showComment <$> xs
  where
    showComment (GHC.L l str) = showOutputable l ++ " " ++ show str

----------------------------------------------------------------------------
-- Helpers

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.
mkComment :: RealLocated String -> RealLocated Comment
mkComment (L l s) =
  L l . Comment . fmap dropTrailing $
    if "{-" `L.isPrefixOf` s
      then case NE.nonEmpty (lines s) of
        Nothing -> s :| []
        Just (x :| xs) ->
          let getIndent y =
                if all isSpace y
                  then startIndent
                  else length (takeWhile isSpace y)
              n = minimum (startIndent : fmap getIndent xs)
           in x :| (drop n <$> xs)
      else s :| []
  where
    dropTrailing = L.dropWhileEnd isSpace
    startIndent = srcSpanStartCol l - 1

-- | Get a 'String' from 'GHC.AnnotationComment'.
unAnnotationComment :: GHC.AnnotationComment -> Maybe String
unAnnotationComment = \case
  GHC.AnnDocCommentNext _ -> Nothing -- @-- |@
  GHC.AnnDocCommentPrev _ -> Nothing -- @-- ^@
  GHC.AnnDocCommentNamed _ -> Nothing -- @-- $@
  GHC.AnnDocSection _ _ -> Nothing -- @-- *@
  GHC.AnnDocOptions s -> Just s
  GHC.AnnLineComment s -> Just s
  GHC.AnnBlockComment s -> Just s

liftMaybe :: Located (Maybe a) -> Maybe (Located a)
liftMaybe = \case
  L _ Nothing -> Nothing
  L l (Just a) -> Just (L l a)

toRealSpan :: Located a -> Maybe (RealLocated a)
toRealSpan (L (RealSrcSpan l) a) = Just (L l a)
toRealSpan _ = Nothing

-- | Detect and extract stack header if it is present.
extractStackHeader ::
  [RealLocated String] ->
  ([RealLocated String], Maybe (RealLocated Comment))
extractStackHeader = \case
  [] -> ([], Nothing)
  (x : xs) ->
    let comment = mkComment x
     in if isStackHeader (unRealSrcSpan comment)
          then (xs, Just comment)
          else (x : xs, Nothing)
  where
    isStackHeader (Comment (x :| _)) =
      "stack" `L.isPrefixOf` dropWhile isSpace (drop 2 x)

-- | Extract pragmas and their associated comments.
extractPragmas ::
  [RealLocated String] ->
  ([RealLocated Comment], [([RealLocated Comment], Pragma)])
extractPragmas = go id id
  where
    go csSoFar pragmasSoFar = \case
      [] -> (csSoFar [], pragmasSoFar [])
      (x : xs) ->
        case parsePragma (unRealSrcSpan x) of
          Nothing -> go (csSoFar . (mkComment x :)) pragmasSoFar xs
          Just pragma ->
            let combined = (csSoFar [], pragma)
             in go id (pragmasSoFar . (combined :)) xs

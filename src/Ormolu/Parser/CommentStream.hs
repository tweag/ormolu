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
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd, isPrefixOf, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified GHC
import qualified Lexer as GHC
import Ormolu.Parser.Pragma
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
  -- | Comment stream, a set of extracted pragmas, and extracted shebangs
  (CommentStream, [Pragma], [Located String])
mkCommentStream extraComments pstate =
  ( CommentStream $
      mkComment <$> sortOn (realSrcSpanStart . getRealSrcSpan) comments,
    pragmas,
    shebangs
  )
  where
    (comments, pragmas) = partitionEithers (partitionComments <$> rawComments)
    rawComments =
      mapMaybe toRealSpan $
        otherExtraComments
          ++ mapMaybe (liftMaybe . fmap unAnnotationComment) (GHC.comment_q pstate)
          ++ concatMap
            (mapMaybe (liftMaybe . fmap unAnnotationComment) . snd)
            (GHC.annotations_comments pstate)
    (shebangs, otherExtraComments) = span isShebang extraComments
    isShebang (L _ str) = "#!" `isPrefixOf` str

-- | Test whether a 'Comment' looks like a Haddock following a definition,
-- i.e. something starting with @-- ^@.
isPrevHaddock :: Comment -> Bool
isPrevHaddock (Comment (x :| _)) = "-- ^" `isPrefixOf` x

-- | Is this comment multiline-style?
isMultilineComment :: Comment -> Bool
isMultilineComment (Comment (x :| _)) = "{-" `isPrefixOf` x

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
    if "{-" `isPrefixOf` s
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
    dropTrailing = dropWhileEnd isSpace
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

-- | If a given comment is a pragma, return it in parsed form in 'Right'.
-- Otherwise return the original comment unchanged.
partitionComments ::
  RealLocated String ->
  Either (RealLocated String) Pragma
partitionComments input =
  case parsePragma (unRealSrcSpan input) of
    Nothing -> Left input
    Just pragma -> Right pragma

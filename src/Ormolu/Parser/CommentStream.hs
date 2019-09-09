{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

-- | Functions for working with comment stream.

module Ormolu.Parser.CommentStream
  ( CommentStream (..)
  , Comment (..)
  , mkCommentStream
  , isPrevHaddock
  , isMultilineComment
  , showCommentStream
  )
where

import Data.Char (isSpace)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sortOn, dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Ormolu.Parser.Pragma
import Ormolu.Utils (showOutputable)
import SrcLoc
import qualified Data.List.NonEmpty as NE
import qualified GHC
import qualified Lexer as GHC

-- | A stream of 'RealLocated' 'Comment's in ascending order with respect to
-- beginning of corresponding spans.

newtype CommentStream = CommentStream [RealLocated Comment]
  deriving (Eq, Data, Semigroup, Monoid)

-- | A wrapper for a single comment. The 'NonEmpty' list inside contains
-- lines of multiline comment @{- … -}@ or just single item\/line otherwise.

newtype Comment = Comment (NonEmpty String)
  deriving (Eq, Show, Data)

-- | Create 'CommentStream' from 'GHC.PState'. We also create a 'Set' of
-- extensions here, which is not sorted in any way. The pragma comment are
-- removed from the 'CommentStream'.

mkCommentStream
  :: [Located String]           -- ^ Extra comments to include
  -> GHC.PState                 -- ^ Parser state to use for comment extraction
  -> (CommentStream, [Pragma])
  -- ^ Comment stream and a set of extracted pragmas
mkCommentStream extraComments pstate =
  ( CommentStream $
      -- NOTE It's easier to normalize pragmas right when we construct comment
      -- streams. Because this way we need to do it only once and when we
      -- perform checking later they'll automatically match.
      mkComment <$> sortOn (realSrcSpanStart . getLoc) comments
  , pragmas
  )
  where
    (comments, pragmas) = partitionEithers (partitionComments <$> rawComments)
    rawComments = mapMaybe toRealSpan $
      extraComments ++
      (fmap unAnnotationComment <$> GHC.comment_q pstate) ++
      concatMap (fmap (fmap unAnnotationComment) . snd)
                (GHC.annotations_comments pstate)

-- | Test whether a 'Comment' looks like a Haddock following a definition,
-- i.e. something starting with @-- ^@.

isPrevHaddock :: Comment -> Bool
isPrevHaddock (Comment (x :| _)) = "-- ^" `isPrefixOf` x

-- | Is this comment multiline-style?

isMultilineComment :: Comment -> Bool
isMultilineComment (Comment (x :| _)) = "{-" `isPrefixOf` x

-- | Pretty-print a 'CommentStream'.

showCommentStream :: CommentStream -> String
showCommentStream (CommentStream xs) = unlines $
  showComment <$> xs
  where
    showComment (GHC.L l str) = showOutputable l ++ " " ++ show str

----------------------------------------------------------------------------
-- Helpers

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.

mkComment :: RealLocated String -> RealLocated Comment
mkComment (L l s) = L l . Comment . fmap dropTrailing $
  if "{-" `isPrefixOf` s
    then case NE.nonEmpty (lines s) of
      Nothing -> s :| []
      Just (x:|xs) ->
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

unAnnotationComment :: GHC.AnnotationComment -> String
unAnnotationComment = \case
  GHC.AnnDocCommentNext s -> s
  GHC.AnnDocCommentPrev s -> s
  GHC.AnnDocCommentNamed s -> s
  GHC.AnnDocSection _ s -> s
  GHC.AnnDocOptions s -> s
  GHC.AnnLineComment s -> s
  GHC.AnnBlockComment s -> s

toRealSpan :: Located a -> Maybe (RealLocated a)
toRealSpan (L (RealSrcSpan l) a) = Just (L l a)
toRealSpan _ = Nothing

-- | If a given comment is a pragma, return it in parsed form in 'Right'.
-- Otherwise return the original comment unchanged.

partitionComments
  :: RealLocated String
  -> Either (RealLocated String) Pragma
partitionComments input =
  case parsePragma (unLoc input) of
    Nothing -> Left input
    Just pragma -> Right pragma

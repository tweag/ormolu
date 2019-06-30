{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Functions for working with comment stream.

module Ormolu.CommentStream
  ( CommentStream (..)
  , Comment (..)
  , mkCommentStream
  , isPrevHaddock
  )
where

import Data.Char (isSpace)
import Data.Data (Data)
import Data.List (isPrefixOf, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
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

-- | Create 'CommentStream' from 'GHC.PState'.

mkCommentStream
  :: [Located String]           -- ^ Extra comments to include
  -> GHC.PState                 -- ^ Parser state to use for comment extraction
  -> CommentStream
mkCommentStream extraComments pstate
  = CommentStream
  -- NOTE It's easier to normalize pragmas right when we construct comment
  -- streams. Because this way we need to do it only once and when we
  -- perform checking later they'll automatically match.
  . fmap (mkComment . (fmap normalizePragma))
  . sortOn startOfSpan
  . mapMaybe toRealSpan $
      extraComments ++
      (fmap unAnnotationComment <$> GHC.comment_q pstate) ++
      concatMap (fmap (fmap unAnnotationComment) . snd)
                (GHC.annotations_comments pstate)
  where
    startOfSpan (L l _) = realSrcSpanStart l
    toRealSpan (L (RealSrcSpan l) a) = Just (L l a)
    toRealSpan _ = Nothing

-- | Test whether a 'Comment' looks like a Haddock following a definition,
-- i.e. something starting with @-- ^@.

isPrevHaddock :: Comment -> Bool
isPrevHaddock (Comment (x :| _)) = "-- ^" `isPrefixOf` x

----------------------------------------------------------------------------
-- Helpers

-- | Normalize pragmas by deleting extra white space.

normalizePragma :: String -> String
normalizePragma x =
  if "{-#" `isPrefixOf` x
    then unwords (words x)
    else x

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.

mkComment :: RealLocated String -> RealLocated Comment
mkComment (L l s) = L l . Comment $
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

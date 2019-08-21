{-# LANGUAGE LambdaCase #-}

module Ormolu.Printer.Comments
  ( insertComments
  ) where

import Control.Applicative
import Data.Maybe
import Data.List
import Debug.Trace
import Ormolu.Parser.CommentStream
import Ormolu.Printer.Internal
import SrcLoc
import qualified Data.Text as T

insertComments :: CommentStream -> SourceMap -> T.Text -> T.Text
insertComments (CommentStream cs) m t =
  traceShow
    (map (\c ->
      ( attach m . srcSpanStartLine $ getLoc c
      , unLoc c
      )) cs)
    ( T.unlines
      . map (\(c, l) -> T.pack (show c) <> T.pack ": " <> l)
      . zip [(0 :: Int)..]
      $ T.lines t
    )

data CommentLocation = After Int
                     | Above Int
                     | Below Int
   deriving Show

attach :: SourceMap -> Int -> Maybe CommentLocation
attach sm l = do
  (After <$> attachAfter sm l)
    <|> (Above <$> attachAbove sm l)
    <|> (Below <$> attachBelow sm l)


attachAfter :: SourceMap -> Int -> Maybe Int
attachAfter (SourceMap sps) line =
  let matching = flip filter sps $ \case
        (s, t) | srcSpanLine s == Just line -> True
        _ -> False
   in listToMaybe
        . map (srcSpanStartLine . snd)
        . sortOn (srcSpanEndCol . fst)
        $ matching

attachAbove :: SourceMap -> Int -> Maybe Int
attachAbove (SourceMap sps) line =
  let matching = flip filter sps $ \case
        (s, t) | srcSpanLine s > Just line -> True
        _ -> False
   in listToMaybe
        . map (srcSpanStartLine . snd)
        . sortOn (\(i, _) -> (srcSpanStartLine i, srcSpanStartCol i))
        $ matching

attachBelow :: SourceMap -> Int -> Maybe Int
attachBelow _ _ = Nothing

srcSpanLine :: RealSrcSpan -> Maybe Int
srcSpanLine s =
  let start = srcSpanStartLine s
      end   = srcSpanEndLine s
   in if start == end then Just start else Nothing

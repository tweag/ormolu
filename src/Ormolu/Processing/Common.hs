{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Common definitions for pre- and post- processing.
module Ormolu.Processing.Common
  ( removeIndentation,
    reindent,
    linesInRegion,
    intSetToRegions,
  )
where

import Data.Char (isSpace)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Config

-- | Remove indentation from a given 'Text'. Return the input with indentation
-- removed and the detected indentation level.
removeIndentation :: Text -> (Text, Int)
removeIndentation (T.lines -> xs) = (T.unlines (T.drop n <$> xs), n)
  where
    n = minimum (getIndent <$> xs)
    getIndent y =
      if T.all isSpace y
        then 0
        else T.length (T.takeWhile isSpace y)

-- | Add indentation to a 'Text'.
reindent :: Int -> Text -> Text
reindent i = T.unlines . fmap (T.replicate i " " <>) . T.lines

-- | All lines in the region specified by 'RegionDeltas'.
linesInRegion :: RegionDeltas -> Text -> Text
linesInRegion RegionDeltas {..} (T.lines -> ls) = T.unlines middle
  where
    (_, nonPrefix) = splitAt regionPrefixLength ls
    middle = take (length nonPrefix - regionSuffixLength) nonPrefix

-- | Convert a set of line indices into disjoint 'RegionDelta's
intSetToRegions ::
  -- | Total number of lines
  Int ->
  IntSet ->
  [RegionDeltas]
intSetToRegions total (IntSet.toAscList -> indices) =
  regionIndicesToDeltas total <$> go Nothing indices
  where
    go Nothing [] = []
    go (Just (a, b)) [] = [RegionIndices (Just a) (Just b)]
    go Nothing (i : is) = go (Just (i, i)) is
    go (Just (a, b)) (i : is)
      | b + 1 == i = go (Just (a, i)) is
      | otherwise = RegionIndices (Just a) (Just b) : go (Just (i, i)) is

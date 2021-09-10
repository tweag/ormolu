{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Common definitions for pre- and post- processing.
module Ormolu.Processing.Common
  ( removeIndentation,
    reindent,
    linesInRegion,
    regionInbetween,
    intSetToRegions,
  )
where

import Data.Char (isSpace)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Config

-- | Remove indentation from a given 'String'. Return the input with
-- indentation removed and the detected indentation level.
removeIndentation :: String -> (String, Int)
removeIndentation (lines -> xs) = (unlines (drop n <$> xs), n)
  where
    n = minimum (getIndent <$> xs)
    getIndent y =
      if all isSpace y
        then 0
        else length (takeWhile isSpace y)

-- | Add indentation to a 'Text'.
reindent :: Int -> Text -> Text
reindent i = T.unlines . fmap (T.replicate i " " <>) . T.lines

-- | All lines in the region specified by 'RegionDeltas'.
linesInRegion :: RegionDeltas -> String -> String
linesInRegion RegionDeltas {..} (lines -> ls) = unlines middle
  where
    (_, nonPrefix) = splitAt regionPrefixLength ls
    middle = take (length nonPrefix - regionSuffixLength) nonPrefix

-- | Get the region after the end of the first and till the end of
-- the second region.
regionInbetween ::
  -- | Total number of lines
  Int ->
  RegionDeltas ->
  RegionDeltas ->
  RegionDeltas
regionInbetween total r r' =
  RegionDeltas
    { regionPrefixLength = total - regionSuffixLength r,
      regionSuffixLength = total - regionPrefixLength r'
    }

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

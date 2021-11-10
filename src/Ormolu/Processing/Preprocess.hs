{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Preprocessing for input source code.
module Ormolu.Processing.Preprocess
  ( preprocess,
  )
where

import Control.Monad
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Config (RegionDeltas (..))
import Ormolu.Processing.Common
import Ormolu.Processing.Cpp

-- | Preprocess the specified region of the input into raw snippets
-- and subregions to be formatted.
preprocess ::
  -- | Whether CPP is enabled
  Bool ->
  RegionDeltas ->
  String ->
  [Either Text RegionDeltas]
preprocess cppEnabled region rawInput = rawSnippetsAndRegionsToFormat
  where
    (linesNotToFormat', replacementLines) = linesNotToFormat cppEnabled region rawInput
    regionsToFormat =
      intSetToRegions rawLineLength $
        IntSet.fromAscList [1 .. rawLineLength] IntSet.\\ linesNotToFormat'
    regionsNotToFormat = intSetToRegions rawLineLength linesNotToFormat'
    -- We want to interleave the regionsToFormat and regionsNotToFormat.
    -- If the first non-formattable region starts at the first line, it is
    -- the first interleaved region, otherwise, we start with the first
    -- region to format.
    interleave' = case regionsNotToFormat of
      r : _ | regionPrefixLength r == 0 -> interleave
      _ -> flip interleave
    rawSnippets = T.pack . flip linesInRegion updatedInput <$> regionsNotToFormat
      where
        updatedInput = unlines . fmap updateLine . zip [1 ..] . lines $ rawInput
        updateLine (i, line) = IntMap.findWithDefault line i replacementLines
    rawSnippetsAndRegionsToFormat =
      interleave' (Left <$> rawSnippets) (Right <$> regionsToFormat)
        >>= patchSeparatingBlankLines
        & dropWhile isBlankRawSnippet
        & L.dropWhileEnd isBlankRawSnippet
    -- For every formattable region, we want to ensure that it is separated by
    -- a blank line from preceding/succeeding raw snippets if it starts/ends
    -- with a blank line.
    -- Empty formattable regions are replaced by a blank line instead.
    -- Extraneous raw snippets at the start/end are dropped afterwards.
    patchSeparatingBlankLines = \case
      Right r@RegionDeltas {..} ->
        if all isSpace (linesInRegion r rawInput)
          then [blankRawSnippet]
          else
            [blankRawSnippet | isBlankLine regionPrefixLength]
              <> [Right r]
              <> [blankRawSnippet | isBlankLine (rawLineLength - regionSuffixLength - 1)]
      Left r -> [Left r]
      where
        blankRawSnippet = Left "\n"
        isBlankLine i = isJust . mfilter (all isSpace) $ rawLines !!? i
    isBlankRawSnippet = \case
      Left r | T.all isSpace r -> True
      _ -> False

    rawLines = A.listArray (0, length rawLines' - 1) rawLines'
      where
        rawLines' = lines rawInput
    rawLineLength = length rawLines

    interleave [] bs = bs
    interleave (a : as) bs = a : interleave bs as

    xs !!? i = if A.bounds rawLines `A.inRange` i then Just $ xs A.! i else Nothing

-- | All lines we are not supposed to format, and a set of replacements
-- for specific lines.
linesNotToFormat ::
  -- | Whether CPP is enabled
  Bool ->
  RegionDeltas ->
  String ->
  (IntSet, IntMap String)
linesNotToFormat cppEnabled region@RegionDeltas {..} input =
  (unconsidered <> magicDisabled <> otherDisabled, lineUpdates)
  where
    unconsidered =
      IntSet.fromAscList $
        [1 .. regionPrefixLength] <> [totalLines - regionSuffixLength + 1 .. totalLines]
    totalLines = length (lines input)
    regionLines = linesInRegion region input
    (magicDisabled, lineUpdates) = magicDisabledLines regionLines
    otherDisabled = (mconcat allLines) regionLines
      where
        allLines = [shebangLines, linePragmaLines] <> [cppLines | cppEnabled]

-- | Ormolu state.
data OrmoluState
  = -- | Enabled
    OrmoluEnabled
  | -- | Disabled
    OrmoluDisabled
  deriving (Eq, Show)

-- | All lines which are disabled by Ormolu's magic comments,
-- as well as normalizing replacements.
magicDisabledLines :: String -> (IntSet, IntMap String)
magicDisabledLines input =
  bimap IntSet.fromAscList IntMap.fromAscList . mconcat $
    go OrmoluEnabled (lines input `zip` [1 ..])
  where
    go _ [] = []
    go state ((line, i) : ls)
      | isMagicComment ormoluDisable line,
        state == OrmoluEnabled =
          ([i], [(i, magicComment ormoluDisable)]) : go OrmoluDisabled ls
      | isMagicComment ormoluEnable line,
        state == OrmoluDisabled =
          ([i], [(i, magicComment ormoluEnable)]) : go OrmoluEnabled ls
      | otherwise = iIfDisabled : go state ls
      where
        iIfDisabled = case state of
          OrmoluDisabled -> ([i], [])
          OrmoluEnabled -> ([], [])

-- | All lines which satisfy a predicate.
linesFiltered :: (String -> Bool) -> String -> IntSet
linesFiltered p =
  IntSet.fromAscList . fmap snd . filter (p . fst) . (`zip` [1 ..]) . lines

-- | Lines which contain a shebang.
shebangLines :: String -> IntSet
shebangLines = linesFiltered ("#!" `L.isPrefixOf`)

-- | Lines which contain a LINE pragma.
linePragmaLines :: String -> IntSet
linePragmaLines = linesFiltered ("{-# LINE" `L.isPrefixOf`)

-- | Inner text of a magic enabling marker.
ormoluEnable :: String
ormoluEnable = "ORMOLU_ENABLE"

-- | Inner text of a magic disabling marker.
ormoluDisable :: String
ormoluDisable = "ORMOLU_DISABLE"

-- | Creates a magic comment with the given inner text.
magicComment :: String -> String
magicComment t = "{- " <> t <> " -}"

-- | Construct a function for whitespace-insensitive matching of string.
isMagicComment ::
  -- | What to expect
  String ->
  -- | String to test
  String ->
  -- | Whether or not the two strings watch
  Bool
isMagicComment expected s0 = isJust $ do
  let trim = dropWhile isSpace
  s1 <- trim <$> L.stripPrefix "{-" (trim s0)
  s2 <- trim <$> L.stripPrefix expected s1
  s3 <- L.stripPrefix "-}" s2
  guard (all isSpace s3)

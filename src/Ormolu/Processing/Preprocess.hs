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
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Ormolu.Config (RegionDeltas (..))
import Ormolu.Processing.Common
import Ormolu.Processing.Cpp

-- | Preprocess the specified region of the input into raw snippets
-- and subregions to be formatted.
preprocess ::
  -- | Whether CPP is enabled
  Bool ->
  RegionDeltas ->
  Text ->
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
    rawSnippets = flip linesInRegion updatedInput <$> regionsNotToFormat
      where
        updatedInput = T.unlines . fmap updateLine . zip [1 ..] . T.lines $ rawInput
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
        if T.all isSpace (linesInRegion r rawInput)
          then [blankRawSnippet]
          else
            [blankRawSnippet | isBlankLine regionPrefixLength]
              <> [Right r]
              <> [blankRawSnippet | isBlankLine (rawLineLength - regionSuffixLength - 1)]
      Left r -> [Left r]
      where
        blankRawSnippet = Left "\n"
        isBlankLine i = isJust . mfilter (T.all isSpace) $ rawLines !!? i
    isBlankRawSnippet = \case
      Left r | T.all isSpace r -> True
      _ -> False

    rawLines = A.listArray (0, length rawLines' - 1) rawLines'
      where
        rawLines' = T.lines rawInput
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
  Text ->
  (IntSet, IntMap Text)
linesNotToFormat cppEnabled region@RegionDeltas {..} input =
  (unconsidered <> magicDisabled <> otherDisabled, lineUpdates)
  where
    unconsidered =
      IntSet.fromAscList $
        [1 .. regionPrefixLength] <> [totalLines - regionSuffixLength + 1 .. totalLines]
    totalLines = length (T.lines input)
    regionLines = linesInRegion region input
    (magicDisabled, lineUpdates) = magicDisabledLines regionLines
    otherDisabled = mconcat allLines regionLines
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
magicDisabledLines :: Text -> (IntSet, IntMap Text)
magicDisabledLines input =
  bimap IntSet.fromAscList IntMap.fromAscList . mconcat $
    go OrmoluEnabled (T.lines input `zip` [1 ..])
  where
    go _ [] = []
    go state ((line, i) : ls)
      | Just rest <- isMagicComment ormoluDisable line,
        state == OrmoluEnabled =
          ([i], [(i, magicComment ormoluDisable <> rest)]) : go OrmoluDisabled ls
      | Just rest <- isMagicComment ormoluEnable line,
        state == OrmoluDisabled =
          ([i], [(i, magicComment ormoluEnable <> rest)]) : go OrmoluEnabled ls
      | otherwise = iIfDisabled : go state ls
      where
        iIfDisabled = case state of
          OrmoluDisabled -> ([i], [])
          OrmoluEnabled -> ([], [])

-- | All lines which satisfy a predicate.
linesFiltered :: (Text -> Bool) -> Text -> IntSet
linesFiltered p =
  IntSet.fromAscList . fmap snd . filter (p . fst) . (`zip` [1 ..]) . T.lines

-- | Lines which contain a shebang.
shebangLines :: Text -> IntSet
shebangLines = linesFiltered ("#!" `T.isPrefixOf`)

-- | Lines which contain a LINE pragma.
linePragmaLines :: Text -> IntSet
linePragmaLines = linesFiltered ("{-# LINE" `T.isPrefixOf`)

-- | Inner text of a magic enabling marker.
ormoluEnable :: Text
ormoluEnable = "ORMOLU_ENABLE"

-- | Inner text of a magic disabling marker.
ormoluDisable :: Text
ormoluDisable = "ORMOLU_DISABLE"

-- | Creates a magic comment with the given inner text.
magicComment :: Text -> Text
magicComment t = "{- " <> t <> " -}"

-- | Construct a function for whitespace-insensitive matching of string.
isMagicComment ::
  -- | What to expect
  Text ->
  -- | String to test
  Text ->
  -- | If the two strings match, we return the rest of the line.
  Maybe Text
isMagicComment expected s0 = do
  s1 <- T.stripStart <$> T.stripPrefix "{-" (T.stripStart s0)
  s2 <- T.stripStart <$> T.stripPrefix expected s1
  T.stripPrefix "-}" s2

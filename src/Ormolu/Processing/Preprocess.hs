{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Preprocessing for input source code.
module Ormolu.Processing.Preprocess
  ( preprocess,
  )
where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Maybe (isJust, maybeToList)
import FastString
import Ormolu.Config (RegionDeltas (..))
import Ormolu.Parser.Shebang (isShebang)
import Ormolu.Processing.Common
import qualified Ormolu.Processing.Cpp as Cpp
import Ormolu.Utils (getIndentationLevel)
import SrcLoc

-- | Transform given input possibly returning comments extracted from it.
-- This handles LINE pragmas, CPP, shebangs, and the magic comments for
-- enabling\/disabling of Ormolu.
preprocess ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Input to process
  String ->
  -- | Region deltas
  RegionDeltas ->
  -- | Literal prefix, pre-processed input, literal suffix, extra comments,
  -- disabled regions
  (String, String, String, [Located String], DisabledRegions)
preprocess path input RegionDeltas {..} =
  go 1 Cpp.Outside id id enabledLines
  where
    (prefixLines, otherLines) = splitAt regionPrefixLength (lines input)
    (regionLines, suffixLines) =
      let regionLength = length otherLines - regionSuffixLength
       in splitAt regionLength otherLines
    (disabledRegions, enabledLines) = cutDisabledRegions regionLines
    go !n cppState inputSoFar csSoFar = \case
      [] ->
        ( unlines prefixLines,
          unlines (inputSoFar []),
          unlines suffixLines,
          csSoFar [],
          disabledRegions
        )
      (x : xs) ->
        let (x', cppState', cs) =
              processLine path n cppState x
         in go
              (n + 1)
              cppState'
              (inputSoFar . (x' :))
              (csSoFar . (maybeToList cs ++))
              xs

-- | Cut regions where formatting is disabled, replacing each of them with
-- a magic comment.
cutDisabledRegions ::
  -- | Lines to process
  [String] ->
  -- | Regions where formatting is disabled, all remaining lines
  (DisabledRegions, [String])
cutDisabledRegions = go (id, id)
  where
    cut xs = (prefix, disabledRegionLines, suffix)
      where
        (prefix, rest) = break isOrmoluDisable xs
        (disabledRegionLines, suffix) = break isOrmoluEnable rest
    removeIndentation xs = drop (getIndentationLevel 0 xs) <$> xs
    go (regionsSoFar, linesSoFar) (cut -> (pre, [], _)) =
      (regionsSoFar [], linesSoFar pre)
    go (regionsSoFar, linesSoFar) (cut -> (pre, _ : dr, [])) =
      (regionsSoFar [removeIndentation dr], linesSoFar $ pre ++ [disableMarker])
    go (regionsSoFar, linesSoFar) (cut -> (pre, _ : dr, _ : suf)) =
      go
        ( regionsSoFar . (removeIndentation dr :),
          linesSoFar . (pre ++) . (disableMarker :) . (enableMarker :)
        )
        suf

-- | Transform a given line possibly returning a comment extracted from it.
processLine ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Line number of this line
  Int ->
  -- | CPP state
  Cpp.State ->
  -- | The actual line
  String ->
  -- | Adjusted line and possibly a comment extracted from it
  (String, Cpp.State, Maybe (Located String))
processLine path n Cpp.Outside line
  | "{-# LINE" `L.isPrefixOf` line =
    let (pragma, res) = getPragma line
        size = length pragma
        ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (size + 1))
     in (res, Cpp.Outside, Just (L ss pragma))
  | isShebang line =
    let ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (length line))
     in ("", Cpp.Outside, Just (L ss line))
  | otherwise =
    let (line', cppState') = Cpp.processLine line Cpp.Outside
     in (line', cppState', Nothing)
  where
    mkSrcLoc' = mkSrcLoc (mkFastString path) n
processLine _ _ cppState line =
  let (line', cppState') = Cpp.processLine line cppState
   in (line', cppState', Nothing)

-- | Take a line pragma and output its replacement (where line pragma is
-- replaced with spaces) and the contents of the pragma itself.
getPragma ::
  -- | Pragma line to analyze
  String ->
  -- | Contents of the pragma and its replacement line
  (String, String)
getPragma [] =
  error "Ormolu.Processing.Preprocess.getPragma: input must not be empty"
getPragma s@(x : xs)
  | "#-}" `L.isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
    let (prag, remline) = getPragma xs
     in (x : prag, ' ' : remline)

-- | Return 'True' if the given string is an enabling marker.
isOrmoluEnable :: String -> Bool
isOrmoluEnable = magicComment "ORMOLU_ENABLE"

-- | Return 'True' if the given string is a disabling marker.
isOrmoluDisable :: String -> Bool
isOrmoluDisable = magicComment "ORMOLU_DISABLE"

-- | Construct a function for whitespace-insensitive matching of string.
magicComment ::
  -- | What to expect
  String ->
  -- | String to test
  String ->
  -- | Whether or not the two strings watch
  Bool
magicComment expected s0 = isJust $ do
  let trim = dropWhile isSpace
  s1 <- trim <$> L.stripPrefix "{-" (trim s0)
  s2 <- trim <$> L.stripPrefix expected s1
  s3 <- L.stripPrefix "-}" s2
  guard (all isSpace s3)

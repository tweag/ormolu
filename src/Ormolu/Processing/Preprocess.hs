{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Preprocessing for input source code.
module Ormolu.Processing.Preprocess
  ( preprocess,
  )
where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Maybe (catMaybes, isJust, maybeToList)
import GHC.Data.FastString
import GHC.Types.SrcLoc
import Ormolu.Config (RegionDeltas (..))
import Ormolu.Parser.Shebang (isShebang)
import qualified Ormolu.Processing.Cpp as Cpp
import Ormolu.Processing.Disabling
import Ormolu.Utils (getIndentationLevel)

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
  (String, String, String, [RealLocated String], DisabledRegions)
preprocess path input RegionDeltas {..} =
  go 1 OrmoluEnabled Cpp.Outside id id id regionLines
  where
    (prefixLines, otherLines) = splitAt regionPrefixLength (lines input)
    (regionLines, suffixLines) =
      let regionLength = length otherLines - regionSuffixLength
       in splitAt regionLength otherLines
    finaliseDisabledRegion r =
      let ls = r []
          n =
            getIndentationLevel 0
              . catMaybes
              $ fmap
                ( \case
                    NoReindent _ -> Nothing
                    Reindent l -> Just l
                )
                ls
       in fmap
            ( \case
                NoReindent l -> NoReindent l
                Reindent l -> Reindent (drop n l)
            )
            ls
    go !n ormoluState cppState inputSoFar csSoFar drsSoFar = \case
      [] ->
        ( unlines prefixLines,
          unlines (inputSoFar []),
          unlines suffixLines,
          csSoFar [],
          drsSoFar $ case ormoluState of
            OrmoluDisabled r -> [finaliseDisabledRegion r]
            OrmoluEnabled -> []
        )
      (x : xs) ->
        let (x', ormoluState', cppState', cs) =
              processLine path n ormoluState cppState x
         in go
              (n + 1)
              ormoluState'
              cppState'
              (inputSoFar . (x' :))
              (csSoFar . (maybeToList cs ++))
              ( case (ormoluState, ormoluState') of
                  (OrmoluDisabled r, OrmoluEnabled) ->
                    drsSoFar . (finaliseDisabledRegion r :)
                  _ ->
                    drsSoFar
              )
              xs

-- | Transform a given line possibly returning a comment extracted from it.
processLine ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Line number of this line
  Int ->
  -- | Whether Ormolu is currently enabled
  OrmoluState ->
  -- | CPP state
  Cpp.State ->
  -- | The actual line
  String ->
  -- | Adjusted line and possibly a comment extracted from it
  (String, OrmoluState, Cpp.State, Maybe (RealLocated String))
processLine path n = processLine'
  where
    mkRealSrcLoc' = mkRealSrcLoc (mkFastString path) n
    processCpp ormoluState line cppState =
      let (line', cppState') = Cpp.processLine line cppState
       in case ormoluState of
            OrmoluEnabled ->
              (line', OrmoluEnabled, cppState', Nothing)
            OrmoluDisabled r ->
              ("", OrmoluDisabled $ r . (Reindent line' :), cppState', Nothing)
    processLine' OrmoluEnabled Cpp.Outside line
      | isLinePragma line =
        let (pragma, res) = getPragma line
            size = length pragma
            ss = mkRealSrcSpan (mkRealSrcLoc' 1) (mkRealSrcLoc' (size + 1))
         in (res, OrmoluEnabled, Cpp.Outside, Just (L ss pragma))
      | isOrmoluEnable line =
        (enableMarker, OrmoluEnabled, Cpp.Outside, Nothing)
      | isOrmoluDisable line =
        (disableMarker, OrmoluDisabled id, Cpp.Outside, Nothing)
      | isShebang line =
        let ss = mkRealSrcSpan (mkRealSrcLoc' 1) (mkRealSrcLoc' (length line))
         in ("", OrmoluEnabled, Cpp.Outside, Just (L ss line))
      | otherwise =
        processCpp OrmoluEnabled line Cpp.Outside
    processLine' (OrmoluDisabled r) Cpp.Outside line
      | isLinePragma line =
        ("", OrmoluDisabled $ r . (NoReindent line :), Cpp.Outside, Nothing)
      | isOrmoluEnable line =
        (enableMarker, OrmoluEnabled, Cpp.Outside, Nothing)
      | otherwise =
        processCpp (OrmoluDisabled r) line Cpp.Outside
    processLine' ormoluState cppState line =
      processCpp ormoluState line cppState

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

-- | Return 'True' if the given string looks like a LINE pragma.
isLinePragma :: String -> Bool
isLinePragma = L.isPrefixOf "{-# LINE"

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

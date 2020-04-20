{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Preprocessing for input source code.
module Ormolu.Processing.Preprocess
  ( preprocess,
  )
where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Maybe (maybeToList)
import FastString
import Ormolu.Parser.Shebang (isShebang)
import Ormolu.Processing.Common
import SrcLoc

-- | Transform given input possibly returning comments extracted from it.
-- This handles LINE pragmas, shebangs, and the magic comments for
-- enabling\/disabling of Ormolu.
preprocess ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Input to process
  String ->
  -- | Adjusted input with comments extracted from it
  (String, [Located String])
preprocess path input = go 1 OrmoluEnabled id id (lines input)
  where
    go !n ormoluState inputSoFar csSoFar = \case
      [] ->
        let input' = unlines (inputSoFar [])
         in ( case ormoluState of
                OrmoluEnabled -> input'
                OrmoluDisabled -> input' ++ endDisabling,
              csSoFar []
            )
      (x : xs) ->
        let (x', ormoluState', cs) = processLine path n ormoluState x
         in go
              (n + 1)
              ormoluState'
              (inputSoFar . (x' :))
              (csSoFar . (maybeToList cs ++))
              xs

-- | Transform a given line possibly returning a comment extracted from it.
processLine ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Line number of this line
  Int ->
  -- | Whether Ormolu is currently enabled
  OrmoluState ->
  -- | The actual line
  String ->
  -- | Adjusted line and possibly a comment extracted from it
  (String, OrmoluState, Maybe (Located String))
processLine path n ormoluState line
  | "{-# LINE" `L.isPrefixOf` line =
    let (pragma, res) = getPragma line
        size = length pragma
        ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (size + 1))
     in (res, ormoluState, Just (L ss pragma))
  | isOrmoluEnable line =
    case ormoluState of
      OrmoluEnabled ->
        (enableMarker, OrmoluEnabled, Nothing)
      OrmoluDisabled ->
        (endDisabling ++ enableMarker, OrmoluEnabled, Nothing)
  | isOrmoluDisable line =
    case ormoluState of
      OrmoluEnabled ->
        (disableMarker ++ startDisabling, OrmoluDisabled, Nothing)
      OrmoluDisabled ->
        (disableMarker, OrmoluDisabled, Nothing)
  | isShebang line =
    let ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (length line))
     in ("", ormoluState, Just (L ss line))
  | otherwise = (line, ormoluState, Nothing)
  where
    mkSrcLoc' = mkSrcLoc (mkFastString path) n

-- | Take a line pragma and output its replacement (where line pragma is
-- replaced with spaces) and the contents of the pragma itself.
getPragma ::
  -- | Pragma line to analyze
  String ->
  -- | Contents of the pragma and its replacement line
  (String, String)
getPragma [] = error "Ormolu.Preprocess.getPragma: input must not be empty"
getPragma s@(x : xs)
  | "#-}" `L.isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
    let (prag, remline) = getPragma xs
     in (x : prag, ' ' : remline)

-- | Canonical enable marker.
enableMarker :: String
enableMarker = "{- ORMOLU_ENABLE -}"

-- | Canonical disable marker.
disableMarker :: String
disableMarker = "{- ORMOLU_DISABLE -}"

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
  s1 <- dropWhile isSpace <$> L.stripPrefix "{-" s0
  s2 <- dropWhile isSpace <$> L.stripPrefix expected s1
  s3 <- L.stripPrefix "-}" s2
  guard (all isSpace s3)

{-# LANGUAGE OverloadedStrings #-}

-- | Postprocessing for the results of printing.
module Ormolu.Processing.Postprocess
  ( postprocess,
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Processing.Common
import qualified Ormolu.Processing.Cpp as Cpp

-- | Postprocess output of the formatter.
postprocess ::
  -- | Desired indentation level
  Int ->
  -- | Regions where formatting was disabled.
  DisabledRegions ->
  -- | Input to process
  Text ->
  Text
postprocess indent disabledRegions =
  T.unlines
    . pasteDisabledRegions
    . fmap (indentLine indent)
    . fmap Cpp.unmaskLine
    . T.lines
  where
    magicComment = (== disableMarker) . T.stripStart
    indentLine n x = T.replicate n " " <> x
    pasteDisabledRegions = paste disabledRegions id
    paste regions'@(r : regions) outputSoFar (l : ls)
      | magicComment l =
        let indentationLevel = indent + T.length (T.takeWhile isSpace l)
         in paste
              regions
              ( outputSoFar
                  . (l :)
                  . ((indentLine indentationLevel . T.pack <$> r) ++)
              )
              ls
      | otherwise = paste regions' (outputSoFar . (l :)) ls
    paste _ outputSoFar remainingLines = outputSoFar remainingLines

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Postprocessing for the results of printing.
module Ormolu.Processing.Postprocess
  ( postprocess,
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ormolu.Processing.Cpp as Cpp
import Ormolu.Processing.Disabling

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
    . fmap (indentLine indent)
    . fmap Cpp.unmaskLine
    . pasteDisabledRegions
    . T.lines
  where
    indentLine n x
      | Cpp.maskPrefix `T.isPrefixOf` x = x
      | otherwise = T.replicate n " " <> x
    pasteDisabledRegions = paste disabledRegions id
      where
        getIndentation = T.length . T.takeWhile isSpace
        isOrmoluDisable = (== disableMarker) . T.stripStart
        isOrmoluEnable = (== enableMarker) . T.stripStart
        paste regions@(r : regions') outputSoFar (l : ls)
          | isOrmoluDisable l =
            let regionIndent = indent + getIndentation l
                ls' = dropWhile (not . isOrmoluEnable) ls
             in paste
                  regions'
                  ( outputSoFar
                      . (l :)
                      . ( fmap
                            ( \case
                                NoReindent s -> T.pack s
                                Reindent s -> indentLine regionIndent $ T.pack s
                            )
                            r
                            ++
                        )
                      . (take 1 ls' ++)
                  )
                  (drop 1 ls')
          | otherwise = paste regions (outputSoFar . (l :)) ls
        paste _ outputSoFar ls = outputSoFar ls

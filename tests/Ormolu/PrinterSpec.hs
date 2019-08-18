{-# LANGUAGE TemplateHaskell #-}

module Ormolu.PrinterSpec (spec) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import Data.Text (Text)
import Ormolu
import Path
import Path.IO
import System.FilePath (addExtension, dropExtensions, splitExtensions)
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as T

spec :: Spec
spec = do
  es <- runIO locateExamples
  forM_ es checkExample

-- | Check a single given example.

checkExample :: Path Rel File -> Spec
checkExample srcPath' = it (fromRelFile srcPath' ++ " works") $ do
  let srcPath = examplesDir </> srcPath'
  expectedOutputPath <- deriveOutput srcPath
  -- 1. Given input snippet of source code parse it and pretty print it.
  -- 2. Parse the result of pretty-printing again and make sure that AST
  --    is the same as AST of the original snippet.
  -- 3. Also check that running the formatter on the output produces the
  --    same output again (the transformation is idempotent).
  -- (These happen in 'ormoluFile' automatically.)
  formatted <-
    ormoluFile
      (defaultConfig { cfgCheckIdempotency = True })
      (fromRelFile srcPath)
  -- 4. Check the output against expected output. Thus all tests should
  --    include two files: input and expected output.
  -- T.writeFile (fromRelFile expectedOutputPath) formatted
  expected <- (liftIO . T.readFile . fromRelFile) expectedOutputPath
  shouldMatch formatted expected

-- | Build list of examples for testing.

locateExamples :: IO [Path Rel File]
locateExamples =
  filter isInput . snd <$> listDirRecurRel examplesDir

-- | Does given path look like input path (as opposed to expected output
-- path)?

isInput :: Path Rel File -> Bool
isInput path =
  let s = fromRelFile path
      (s', exts) = splitExtensions s
  in exts == ".hs" && not ("-out" `isSuffixOf` s')

-- | For given path of input file return expected name of output.

deriveOutput :: Path Rel File -> IO (Path Rel File)
deriveOutput path = parseRelFile $
  addExtension (dropExtensions (fromRelFile path) ++ "-out") "hs"

-- | A version of 'shouldBe' that is specialized to comparing 'Text' values.
-- It also prints multi-line snippets in a more readable form.

shouldMatch :: Text -> Text -> Expectation
shouldMatch actual expected  =
  when (actual /= expected) . expectationFailure $ unlines
    [ ">>>>>>>>>>>>>>>>>>>>>> expected:"
    , T.unpack expected
    , ">>>>>>>>>>>>>>>>>>>>>> but got:"
    , T.unpack actual
    ]

examplesDir :: Path Rel Dir
examplesDir = $(mkRelDir "data/examples")

{-# LANGUAGE TemplateHaskell #-}

module Ormolu.PrinterSpec
  ( spec,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Ormolu
import Path
import Path.IO
import qualified System.FilePath as F
import Test.Hspec

spec :: Spec
spec = do
  es <- runIO locateExamples
  forM_ es checkExample

-- | Check a single given example.
checkExample :: Path Rel File -> Spec
checkExample srcPath' = it (fromRelFile srcPath' ++ " works") . withNiceExceptions $ do
  let srcPath = examplesDir </> srcPath'
  expectedOutputPath <- deriveOutput srcPath
  -- 1. Given input snippet of source code parse it and pretty print it.
  -- 2. Parse the result of pretty-printing again and make sure that AST
  -- is the same as AST of the original snippet. (This happens in
  -- 'ormoluFile' automatically.)
  formatted0 <- ormoluFile defaultConfig (fromRelFile srcPath)
  -- 3. Check the output against expected output. Thus all tests should
  -- include two files: input and expected output.
  -- T.writeFile (fromRelFile expectedOutputPath) formatted0
  expected <- (liftIO . T.readFile . fromRelFile) expectedOutputPath
  shouldMatch False formatted0 expected
  -- 4. Check that running the formatter on the output produces the same
  -- output again (the transformation is idempotent).
  formatted1 <- ormolu defaultConfig "<formatted>" (T.unpack formatted0)
  shouldMatch True formatted1 formatted0

-- | Build list of examples for testing.
locateExamples :: IO [Path Rel File]
locateExamples =
  filter isInput . snd <$> listDirRecurRel examplesDir

-- | Does given path look like input path (as opposed to expected output
-- path)?
isInput :: Path Rel File -> Bool
isInput path =
  let s = fromRelFile path
      (s', exts) = F.splitExtensions s
   in exts == ".hs" && not ("-out" `isSuffixOf` s')

-- | For given path of input file return expected name of output.
deriveOutput :: Path Rel File -> IO (Path Rel File)
deriveOutput path =
  parseRelFile $
    F.addExtension (F.dropExtensions (fromRelFile path) ++ "-out") "hs"

-- | A version of 'shouldBe' that is specialized to comparing 'Text' values.
-- It also prints multi-line snippets in a more readable form.
shouldMatch :: Bool -> Text -> Text -> Expectation
shouldMatch idempotencyTest actual expected =
  when (actual /= expected) . expectationFailure $
    unlines
      [ ">>>>>>>>>>>>>>>>>>>>>> expected (" ++ pass ++ "):",
        T.unpack expected,
        ">>>>>>>>>>>>>>>>>>>>>> but got:",
        T.unpack actual
      ]
  where
    pass =
      if idempotencyTest
        then "idempotency pass"
        else "first pass"

examplesDir :: Path Rel Dir
examplesDir = $(mkRelDir "data/examples")

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely using 'displayException'.
withNiceExceptions ::
  -- | Action that may throw the exception
  Expectation ->
  Expectation
withNiceExceptions m = m `catch` h
  where
    h :: OrmoluException -> IO ()
    h = expectationFailure . displayException

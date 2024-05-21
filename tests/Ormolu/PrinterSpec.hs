{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.PrinterSpec (spec) where

import Control.Exception
import Control.Monad
import Data.List (isSuffixOf)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T.Utf8
import Ormolu
import Ormolu.Fixity
import Path
import Path.IO
import System.Environment (lookupEnv)
import System.FilePath qualified as F
import Test.Hspec

spec :: Spec
spec = do
  es <- runIO locateExamples
  forM_ es checkExample

-- | Fixity overrides that are to be used with the test examples.
testsuiteOverrides :: FixityOverrides
testsuiteOverrides =
  FixityOverrides
    ( Map.fromList
        [ (".=", FixityInfo InfixR 8),
          ("#", FixityInfo InfixR 5)
        ]
    )

-- | Check a single given example.
checkExample :: Path Rel File -> Spec
checkExample srcPath' = it (fromRelFile srcPath' ++ " works") . withNiceExceptions $ do
  let srcPath = examplesDir </> srcPath'
      inputPath = fromRelFile srcPath
      config =
        defaultConfig
          { cfgSourceType = detectSourceType inputPath,
            cfgFixityOverrides = testsuiteOverrides,
            cfgDependencies =
              Set.fromList
                [ "base",
                  "esqueleto",
                  "hspec",
                  "lens",
                  "servant"
                ]
          }
  expectedOutputPath <- deriveOutput srcPath
  -- 1. Given input snippet of source code parse it and pretty print it.
  -- 2. Parse the result of pretty-printing again and make sure that AST
  -- is the same as AST of the original snippet. (This happens in
  -- 'ormoluFile' automatically.)
  formatted0 <- ormoluFile config inputPath
  -- 3. Check the output against expected output. Thus all tests should
  -- include two files: input and expected output.
  whenShouldRegenerateOutput $
    T.Utf8.writeFile (fromRelFile expectedOutputPath) formatted0
  expected <- T.Utf8.readFile $ fromRelFile expectedOutputPath
  shouldMatch False formatted0 expected
  -- 4. Check that running the formatter on the output produces the same
  -- output again (the transformation is idempotent).
  formatted1 <- ormolu config "<formatted>" formatted0
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
   in exts `elem` [".hs", ".hsig"] && not ("-out" `isSuffixOf` s')

-- | For given path of input file return expected name of output.
deriveOutput :: Path Rel File -> IO (Path Rel File)
deriveOutput path =
  parseRelFile $
    F.addExtension (radical ++ "-out") exts
  where
    (radical, exts) = F.splitExtensions (fromRelFile path)

-- | A version of 'shouldBe' that is specialized to comparing 'Text' values.
-- It also prints multi-line snippets in a more readable form.
shouldMatch :: Bool -> Text -> Text -> Expectation
shouldMatch idempotenceTest actual expected =
  when (actual /= expected) . expectationFailure $
    unlines
      [ ">>>>>>>>>>>>>>>>>>>>>> expected (" ++ pass ++ "):",
        T.unpack expected,
        ">>>>>>>>>>>>>>>>>>>>>> but got:",
        T.unpack actual
      ]
  where
    pass =
      if idempotenceTest
        then "idempotence pass"
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

whenShouldRegenerateOutput :: IO () -> IO ()
whenShouldRegenerateOutput action = do
  shouldRegenerateOutput <- isJust <$> lookupEnv "ORMOLU_REGENERATE_EXAMPLES"
  when shouldRegenerateOutput action

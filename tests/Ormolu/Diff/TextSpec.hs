{-# LANGUAGE TemplateHaskell #-}

module Ormolu.Diff.TextSpec (spec) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Ormolu.Diff.Text
import Ormolu.Terminal
import Path
import Path.IO
import qualified System.FilePath as FP
import System.IO (hClose)
import Test.Hspec

spec :: Spec
spec =
  describe "printTextDiff" $ do
    stdTest "one-line-added" "empty" "one-line"
    stdTest "one-line-removed" "one-line" "empty"
    stdTest "no-preceding" "main-foo" "main"
    stdTest "no-following" "main" "main-v2"
    stdTest "simple-hunk" "main-and-foo" "main-and-foo-v2"
    stdTest "joined-hunk" "main-and-bar" "main-and-bar-v2"
    stdTest "two-hunks" "main-and-baz" "main-and-baz-v2"
    stdTest "trimming" "spaced" "spaced-v2"

-- | Test diff printig.
stdTest ::
  -- | Name of the test case
  String ->
  -- | Location of input A
  FilePath ->
  -- | Location of input B
  FilePath ->
  Spec
stdTest name pathA pathB = it name $ do
  inputA <-
    parseRelFile (FP.addExtension pathA "hs")
      >>= T.readFile . toFilePath . (diffInputsDir </>)
  inputB <-
    parseRelFile (FP.addExtension pathB "hs")
      >>= T.readFile . toFilePath . (diffInputsDir </>)
  let expectedDiffPath = FP.addExtension name "txt"
  expectedDiffText <-
    parseRelFile expectedDiffPath
      >>= T.readFile . toFilePath . (diffOutputsDir </>)
  let Just actualDiff = diffText inputA inputB "TEST"
  actualDiffText <- printDiff actualDiff
  actualDiffText `shouldBe` expectedDiffText

-- | Print to a 'Text' value.
printDiff :: TextDiff -> IO Text
printDiff diff =
  withSystemTempFile "ormolu-diff-test" $ \path h -> do
    runTerm (printTextDiff diff) Never h
    hClose h
    T.readFile (toFilePath path)

diffTestsDir :: Path Rel Dir
diffTestsDir = $(mkRelDir "data/diff-tests")

diffInputsDir :: Path Rel Dir
diffInputsDir = diffTestsDir </> $(mkRelDir "inputs")

diffOutputsDir :: Path Rel Dir
diffOutputsDir = diffTestsDir </> $(mkRelDir "outputs")

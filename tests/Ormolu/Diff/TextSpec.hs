{-# LANGUAGE TemplateHaskell #-}

module Ormolu.Diff.TextSpec (spec) where

import Data.Text.IO.Utf8 qualified as T.Utf8
import Ormolu.Diff.Text
import Ormolu.Terminal
import Path
import System.FilePath qualified as FP
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
    stdTest "trailing-blank-line" "no-trailing-blank-line" "with-trailing-blank-line"
    stdTest "trimming-trailing-both-eof" "applicative-before" "applicative-after"
    stdTest "trimming-trailing-both-out-of-margin" "longer" "longer-v2"

-- | Test diff printing.
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
      >>= T.Utf8.readFile . toFilePath . (diffInputsDir </>)
  inputB <-
    parseRelFile (FP.addExtension pathB "hs")
      >>= T.Utf8.readFile . toFilePath . (diffInputsDir </>)
  let expectedDiffPath = FP.addExtension name "txt"
  expectedDiffText <-
    parseRelFile expectedDiffPath
      >>= T.Utf8.readFile . toFilePath . (diffOutputsDir </>)
  Just actualDiff <- pure $ diffText inputA inputB "TEST"
  runTermPure (printTextDiff actualDiff) `shouldBe` expectedDiffText

diffTestsDir :: Path Rel Dir
diffTestsDir = $(mkRelDir "data/diff-tests")

diffInputsDir :: Path Rel Dir
diffInputsDir = diffTestsDir </> $(mkRelDir "inputs")

diffOutputsDir :: Path Rel Dir
diffOutputsDir = diffTestsDir </> $(mkRelDir "outputs")

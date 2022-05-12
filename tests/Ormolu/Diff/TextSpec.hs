{-# LANGUAGE TemplateHaskell #-}

module Ormolu.Diff.TextSpec (spec) where

import Data.Text (Text)
import Ormolu.Diff.Text
import Ormolu.Terminal
import Ormolu.Utils.IO
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
      >>= readFileUtf8 . toFilePath . (diffInputsDir </>)
  inputB <-
    parseRelFile (FP.addExtension pathB "hs")
      >>= readFileUtf8 . toFilePath . (diffInputsDir </>)
  let expectedDiffPath = FP.addExtension name "txt"
  expectedDiffText <-
    parseRelFile expectedDiffPath
      >>= readFileUtf8 . toFilePath . (diffOutputsDir </>)
  Just actualDiff <- pure $ diffText inputA inputB "TEST"
  actualDiffText <- printDiff actualDiff
  actualDiffText `shouldBe` expectedDiffText

-- | Print to a 'Text' value.
printDiff :: TextDiff -> IO Text
printDiff diff =
  withSystemTempFile "ormolu-diff-test" $ \path h -> do
    runTerm (printTextDiff diff) Never h
    hClose h
    readFileUtf8 (toFilePath path)

diffTestsDir :: Path Rel Dir
diffTestsDir = $(mkRelDir "data/diff-tests")

diffInputsDir :: Path Rel Dir
diffInputsDir = diffTestsDir </> $(mkRelDir "inputs")

diffOutputsDir :: Path Rel Dir
diffOutputsDir = diffTestsDir </> $(mkRelDir "outputs")

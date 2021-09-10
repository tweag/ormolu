{-# LANGUAGE LambdaCase #-}

module Ormolu.Parser.ParseFailureSpec (spec) where

import Ormolu
import Ormolu.Utils (showOutputable)
import System.FilePath
import Test.Hspec

spec :: Spec
spec = do
  "disabling-preserves-error-location.hs" `failsAt` "12:1"
  "line-pragma.hs" `failsAt` "4:47"

failsAt :: String -> String -> Spec
failsAt filename location =
  let filePath = baseDir </> filename
   in it (filename ++ " fails at " ++ location) $
        ormoluFile defaultConfig filePath
          `shouldThrow` \case
            OrmoluParsingFailed srcSpan _ ->
              showOutputable srcSpan == filePath ++ ":" ++ location
            _ -> False

baseDir :: FilePath
baseDir = "data" </> "parse-failures"

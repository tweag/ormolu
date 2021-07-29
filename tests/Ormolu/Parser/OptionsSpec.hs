{-# LANGUAGE LambdaCase #-}

module Ormolu.Parser.OptionsSpec (spec) where

import qualified Data.Text as T
import Ormolu
import Test.Hspec

spec :: Spec
spec = describe "GHC options in source files take priority" $ do
  it "default extensions can be disabled locally" $ do
    let src =
          unlines
            [ "{-# LANGUAGE NoBlockArguments #-}",
              "",
              "test = test do test"
            ]
    fixedPoint [] src `shouldThrow` \case
      OrmoluParsingFailed {} -> True
      _ -> False
  it "extensions disabled via CLI can be enabled locally" $ do
    let src =
          unlines
            [ "{-# LANGUAGE BlockArguments #-}",
              "",
              "test = test do test"
            ]
    fixedPoint ["-XNoBlockArguments"] src
  where
    fixedPoint opts input = do
      output <- ormolu defaultConfig {cfgDynOptions = DynOption <$> opts} "<input>" input
      T.unpack output `shouldBe` input

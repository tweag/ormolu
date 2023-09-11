module Main where

import Ormolu.Config qualified as Ormolu
import Ormolu.Logging (initializeLogging)
import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = do
  initializeLogging Ormolu.defaultConfig
  hspec Spec.spec

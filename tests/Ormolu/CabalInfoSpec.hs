{-# LANGUAGE RecordWildCards #-}

module Ormolu.CabalInfoSpec (spec) where

import Data.List (sort)
import Ormolu.Config (DynOption (..))
import Ormolu.Utils.Cabal
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "findCabalFile" $ do
    let findsOrmoluCabal start expectedCabalFile = do
          Just absolute <- findCabalFile start
          absolute `shouldSatisfy` isAbsolute
          makeRelativeToCurrentDirectory absolute `shouldReturn` expectedCabalFile
    it "it returns correct absolute path" $
      findsOrmoluCabal "src/Ormolu/Config.hs" "ormolu.cabal"
    it "it finds correct path even when it starts from nonsense" $
      findsOrmoluCabal "a/b/c/d/e" "ormolu.cabal"
    it "returns Nothing when it cannot find a cabal file" $
      findCabalFile "/foo.hs" `shouldReturn` Nothing
    it "does not consider directories as .cabal files" $
      withSystemTempDirectory "" $
        \dir -> do
          createDirectory $ dir </> ".cabal"
          cabalFile <- findCabalFile (dir </> "foo/bar.hs")
          cabalFile `shouldBe` Nothing
  describe "parseCabalInfo" $ do
    it "extracts correct package name from ormolu.cabal" $ do
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal" "src/Ormolu/Config.hs"
      ciPackageName `shouldBe` Just "ormolu"
    it "extracts correct dyn opts from ormolu.cabal" $ do
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal" "src/Ormolu/Config.hs"
      ciDynOpts `shouldBe` [DynOption "-XHaskell2010"]
    it "extracts correct dependencies from ormolu.cabal (src/Ormolu/Config.hs)" $ do
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal" "src/Ormolu/Config.hs"
      sort ciDependencies `shouldBe` ["Cabal", "Diff", "aeson", "ansi-terminal", "array", "base", "bytestring", "containers", "directory", "dlist", "exceptions", "file-embed", "filepath", "ghc-lib-parser", "mtl", "syb", "template-haskell", "text", "th-lift-instances"]
    it "extracts correct dependencies from ormolu.cabal (tests/Ormolu/PrinterSpec.hs)" $ do
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal" "tests/Ormolu/PrinterSpec.hs"
      sort ciDependencies `shouldBe` ["base", "containers", "directory", "filepath", "ghc-lib-parser", "hspec", "ormolu", "path", "path-io", "temporary", "text"]

{-# LANGUAGE RecordWildCards #-}

module Ormolu.CabalInfoSpec (spec) where

import qualified Data.Set as Set
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
      ciDependencies `shouldBe` Set.fromList ["Cabal-syntax", "Diff", "MemoTrie", "aeson", "ansi-terminal", "array", "base", "bytestring", "containers", "directory", "dlist", "exceptions", "file-embed", "filepath", "ghc-lib-parser", "megaparsec", "mtl", "syb", "template-haskell", "text", "th-lift-instances"]
    it "extracts correct dependencies from ormolu.cabal (tests/Ormolu/PrinterSpec.hs)" $ do
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal" "tests/Ormolu/PrinterSpec.hs"
      ciDependencies `shouldBe` Set.fromList ["QuickCheck", "base", "containers", "directory", "filepath", "ghc-lib-parser", "hspec", "hspec-megaparsec", "megaparsec", "ormolu", "path", "path-io", "temporary", "text"]

    it "handles `hs-source-dirs: .`" $ do
      CabalInfo {..} <- parseTestCabalInfo "Foo.hs"
      ciDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
    it "handles empty hs-source-dirs" $ do
      CabalInfo {..} <- parseTestCabalInfo "Bar.hs"
      ciDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
  where
    parseTestCabalInfo f =
      parseCabalInfo "data/cabal-tests/test.cabal" ("data/cabal-tests" </> f)

{-# LANGUAGE RecordWildCards #-}

module Ormolu.CabalInfoSpec (spec) where

import Data.Set qualified as Set
import Distribution.Types.PackageName (unPackageName)
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
    it "returns correct absolute path" $
      findsOrmoluCabal "src/Ormolu/Config.hs" "ormolu.cabal"
    it "finds correct path even when it starts from nonsense" $
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
    it "extracts correct cabal info from ormolu.cabal for src/Ormolu/Config.hs" $ do
      (mentioned, CabalInfo {..}) <- parseCabalInfo "ormolu.cabal" "src/Ormolu/Config.hs"
      mentioned `shouldBe` True
      unPackageName ciPackageName `shouldBe` "ormolu"
      ciDynOpts `shouldBe` [DynOption "-XGHC2021"]
      Set.map unPackageName ciDependencies `shouldBe` Set.fromList ["Cabal-syntax", "Diff", "MemoTrie", "ansi-terminal", "array", "base", "binary", "bytestring", "choice", "containers", "directory", "file-embed", "filepath", "ghc-lib-parser", "megaparsec", "mtl", "syb", "text"]
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "ormolu.cabal"
    it "extracts correct cabal info from ormolu.cabal for tests/Ormolu/PrinterSpec.hs" $ do
      (mentioned, CabalInfo {..}) <- parseCabalInfo "ormolu.cabal" "tests/Ormolu/PrinterSpec.hs"
      mentioned `shouldBe` True
      unPackageName ciPackageName `shouldBe` "ormolu"
      ciDynOpts `shouldBe` [DynOption "-XGHC2021"]
      Set.map unPackageName ciDependencies `shouldBe` Set.fromList ["Cabal-syntax", "QuickCheck", "base", "choice", "containers", "directory", "filepath", "ghc-lib-parser", "hspec", "hspec-megaparsec", "megaparsec", "ormolu", "path", "path-io", "temporary", "text"]
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "ormolu.cabal"
    it "handles correctly files that are not mentioned in ormolu.cabal" $ do
      (mentioned, CabalInfo {..}) <- parseCabalInfo "ormolu.cabal" "src/FooBob.hs"
      mentioned `shouldBe` False
      unPackageName ciPackageName `shouldBe` "ormolu"
      ciDynOpts `shouldBe` []
      Set.map unPackageName ciDependencies `shouldBe` Set.fromList ["base"]
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "ormolu.cabal"
    it "handles `hs-source-dirs: .`" $ do
      (_, CabalInfo {..}) <- parseTestCabalInfo "Foo.hs"
      ciDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
    it "handles empty hs-source-dirs" $ do
      (_, CabalInfo {..}) <- parseTestCabalInfo "Bar.hs"
      ciDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
  where
    parseTestCabalInfo f =
      parseCabalInfo "data/cabal-tests/test.cabal" ("data/cabal-tests" </> f)

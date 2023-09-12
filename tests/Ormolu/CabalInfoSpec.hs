{-# LANGUAGE LambdaCase #-}
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
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal"
      unPackageName ciPackageName `shouldBe` "ormolu"
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "ormolu.cabal"
      StanzaInfo {..} <- lookupStanzaInfo' "src/Ormolu/Config.hs" ciStanzaInfoMap
      siDynOpts `shouldBe` [DynOption "-XGHC2021"]
      Set.map unPackageName siDependencies `shouldBe` Set.fromList ["Cabal-syntax", "Diff", "MemoTrie", "ansi-terminal", "array", "base", "binary", "bytestring", "containers", "deepseq", "directory", "file-embed", "filepath", "ghc-lib-parser", "megaparsec", "mtl", "syb", "text"]
    it "extracts correct cabal info from ormolu.cabal for tests/Ormolu/PrinterSpec.hs" $ do
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal"
      unPackageName ciPackageName `shouldBe` "ormolu"
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "ormolu.cabal"
      StanzaInfo {..} <- lookupStanzaInfo' "tests/Ormolu/PrinterSpec.hs" ciStanzaInfoMap
      siDynOpts `shouldBe` [DynOption "-XGHC2021"]
      Set.map unPackageName siDependencies `shouldBe` Set.fromList ["Cabal-syntax", "QuickCheck", "base", "containers", "directory", "filepath", "ghc-lib-parser", "hspec", "hspec-megaparsec", "megaparsec", "ormolu", "path", "path-io", "temporary", "text"]
    it "handles correctly files that are not mentioned in ormolu.cabal" $ do
      CabalInfo {..} <- parseCabalInfo "ormolu.cabal"
      unPackageName ciPackageName `shouldBe` "ormolu"
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "ormolu.cabal"
      mStanzaInfo <- lookupStanzaInfo "src/FooBob.hs" ciStanzaInfoMap
      mStanzaInfo `shouldBe` Nothing
    it "handles `hs-source-dirs: .`" $ do
      CabalInfo {..} <- parseCabalInfo "data/cabal-tests/test.cabal"
      StanzaInfo {..} <- lookupStanzaInfo' "data/cabal-tests/Foo.hs" ciStanzaInfoMap
      siDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
    it "handles empty hs-source-dirs" $ do
      CabalInfo {..} <- parseCabalInfo "data/cabal-tests/test.cabal"
      StanzaInfo {..} <- lookupStanzaInfo' "data/cabal-tests/Bar.hs" ciStanzaInfoMap
      siDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
  where
    lookupStanzaInfo' fp stanzaInfoMap =
      lookupStanzaInfo fp stanzaInfoMap >>= \case
        Nothing -> error $ "StanzaInfoMap did not contain: " ++ fp
        Just info -> pure info

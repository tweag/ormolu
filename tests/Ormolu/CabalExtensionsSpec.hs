module Ormolu.CabalExtensionsSpec (spec) where

import qualified Data.Map as M
import Ormolu.Config
import Ormolu.Utils.Cabal
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Handle extensions from .cabal files" $ do
  it "extract extensions from .cabal files" $ do
    extsByFile <- getExtensionsFromCabalFile "ormolu.cabal"
    extsByFile `shouldNotSatisfy` M.null
    M.elems extsByFile `shouldSatisfy` all (== [DynOption "-XHaskell2010"])
    extsByFile
      `shouldSatisfy` members
        [ "./src/Ormolu",
          "./app/Main",
          "./tests/Spec"
        ]
  it "find the .cabal file" $ do
    let findsOrmoluCabal start expectedCabalFile = do
          cabalFile <- findCabalFile start
          cabalFile `shouldBe` Just expectedCabalFile
    findsOrmoluCabal "src/Ormolu/Config.hs" "./ormolu.cabal"
    findsOrmoluCabal "a/b/c/d/e" "./ormolu.cabal"
  it "do not consider directories as .cabal files" $
    withSystemTempDirectory "" $ \dir -> do
      createDirectory $ dir </> ".cabal"
      cabalFile <- findCabalFile $ dir </> "foo/bar.hs"
      cabalFile `shouldBe` Nothing
  where
    members as m = all (`M.member` m) as

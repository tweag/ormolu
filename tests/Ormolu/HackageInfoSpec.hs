module Ormolu.HackageInfoSpec where

import qualified Data.HashMap.Strict as HashMap
import GHC.Types.Fixity
import Ormolu.Fixity
import Test.Hspec

spec :: Spec
spec = do
  check1

-- See https://gist.github.com/tbagrel1/fa785b38433393dcefc2465b7338c8be

-- #########################################################################
-- tests with buildFixityMap'
-- #########################################################################

(|->) :: a -> b -> (a, b)
a |-> b = (a, b)

infixr 0 |->

checkFixityMap' ::
  [(String, [(String, FixityInfo)])] ->
  [(String, Int)] ->
  [String] ->
  Float ->
  [(String, FixityInfo)] ->
  Expectation
checkFixityMap' lPackageToOps lPackageToPopularity cabalDependencies threshold lResult =
  buildFixityMap' packageToOps packageToPopularity cabalDependencies threshold `shouldBe` result
  where
    packageToOps = HashMap.map HashMap.fromList . HashMap.fromList $ lPackageToOps
    packageToPopularity = HashMap.fromList lPackageToPopularity
    result = HashMap.fromList lResult

-- conflict in unspecified packages, < threshold
check1 :: Spec
check1 = it "correctly merges fixities when a conflict appears in unspecified packages, with max(pop) < threshold" $ do
  let packageToOps =
        [ "A" |-> ["+" |-> FixityInfo (Just InfixL) 4 4],
          "B" |-> ["+" |-> FixityInfo (Just InfixR) 6 6]
        ]
      packageToPopularity =
        [ "A" |-> 3,
          "B" |-> 5
        ]
      cabalDependencies = []
      threshold = 0.9
      result =
        [ "+" |-> FixityInfo Nothing 4 6
        ]
  checkFixityMap' packageToOps packageToPopularity cabalDependencies threshold result

-- conflict in unspecified packages, > threshold with 1 winner

-- conflict in unspecified packages, > threshold with 2 winners

-- conflict in unspecified packages, threshold == 0 -> keep best

-- conflict in unspecified packages, threshold == 1 -> merge all

-- conflict in cabal dependencies

-- base added if not specified

-- base override cabal Dependencies

-- cabal dependencies override unspecified packages

-- complex case mixing all these tests

-- #########################################################################
-- tests with buildFixityMap (actual Hackage database)
-- #########################################################################

-- ":" (R, 5, 5)

-- "<|>" (L, 3, 3)

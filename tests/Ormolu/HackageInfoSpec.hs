module Ormolu.HackageInfoSpec where

import qualified Data.HashMap.Strict as HashMap
import Ormolu.Fixity
import Test.Hspec

spec :: Spec
spec = do
  check1
  check2
  check3
  check4
  check5
  check6
  check7
  check8
  check9

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
checkFixityMap' lPackageToOps lPackageToPopularity cabalDependencies threshold lExpectedResult =
  filteredResultMap `shouldBe` expectedResult
  where
    keysToCheck = HashMap.keys expectedResult
    filteredResultMap = HashMap.filterWithKey (\k _ -> k `elem` keysToCheck) resultMap
    resultMap = buildFixityMap' lPackageToOps' lPackageToPopularity' cabalDependencies threshold
    lPackageToOps' = HashMap.map HashMap.fromList . HashMap.fromList $ lPackageToOps
    lPackageToPopularity' = HashMap.fromList lPackageToPopularity
    expectedResult = HashMap.fromList lExpectedResult

-- conflict in unspecified packages, < threshold
check1 :: Spec
check1 = it "correctly merges fixities when a conflict appears in unspecified packages, with max(pop) < threshold" $ do
  let operators =
        [ "A" |-> ["+" |-> FixityInfo (Just InfixL) 4 4],
          "B" |-> ["+" |-> FixityInfo (Just InfixR) 6 6]
        ]
      popularity =
        [ "A" |-> 3,
          "B" |-> 5
        ]
      cabalDependencies = []
      threshold = 0.9
      result =
        [ "+" |-> FixityInfo Nothing 4 6
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

-- conflict in unspecified packages, >= threshold with 1 winner
check2 :: Spec
check2 = it "keeps only the most popular declaration when a conflict appears in unspecified packages, with max(pop) >= threshold" $ do
  let operators =
        [ "A" |-> ["+" |-> FixityInfo (Just InfixL) 4 4],
          "B" |-> ["+" |-> FixityInfo (Just InfixR) 6 6]
        ]
      popularity =
        [ "A" |-> 5,
          "B" |-> 103
        ]
      cabalDependencies = []
      threshold = 0.9
      result =
        [ "+" |-> FixityInfo (Just InfixR) 6 6
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

-- conflict in unspecified packages, > threshold with 2 winners
check3 :: Spec
check3 = it "merges the ex-aequo most popular declaration when a conflict appears in unspecified packages, with max(pop) >= threshold" $ do
  let operators =
        [ "A" |-> ["+" |-> FixityInfo (Just InfixL) 4 4],
          "B" |-> ["+" |-> FixityInfo (Just InfixR) 6 6],
          "C" |-> ["+" |-> FixityInfo (Just InfixR) 8 8]
        ]
      popularity =
        [ "A" |-> 5,
          "B" |-> 103,
          "C" |-> 103
        ]
      cabalDependencies = []
      threshold = 0.4
      result =
        [ "+" |-> FixityInfo (Just InfixR) 6 8
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

-- conflict in unspecified packages, threshold == 0 -> keep best
check4 :: Spec
check4 = it "keeps only the most popular declaration when a conflict appears in unspecified packages, threshold == 0" $ do
  let operators =
        [ "A" |-> ["+" |-> FixityInfo (Just InfixL) 4 4],
          "B" |-> ["+" |-> FixityInfo (Just InfixR) 6 6]
        ]
      popularity =
        [ "A" |-> 5,
          "B" |-> 103
        ]
      cabalDependencies = []
      threshold = 0.0
      result =
        [ "+" |-> FixityInfo (Just InfixR) 6 6
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

-- conflict in unspecified packages, threshold == 1 -> merge all
check5 :: Spec
check5 = it "merges all declarations when a conflict appears in unspecified packages, threshold == 1" $ do
  let operators =
        [ "A" |-> ["+" |-> FixityInfo (Just InfixN) 4 4],
          "B" |-> ["+" |-> FixityInfo (Just InfixN) 6 6],
          "C" |-> ["+" |-> FixityInfo (Just InfixN) 8 8]
        ]
      popularity =
        [ "A" |-> 5,
          "B" |-> 103,
          "C" |-> 11103
        ]
      cabalDependencies = []
      threshold = 1.0
      result =
        [ "+" |-> FixityInfo (Just InfixN) 4 8
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

-- conflict in cabal dependencies
check6 :: Spec
check6 = it "merges all declarations when a conflict appears in cabal dependencies" $ do
  let operators =
        [ "A" |-> ["+" |-> FixityInfo (Just InfixR) 4 4, "-" |-> FixityInfo (Just InfixR) 2 2],
          "B" |-> ["+" |-> FixityInfo (Just InfixN) 6 6, "-" |-> FixityInfo (Just InfixL) 4 4],
          "C" |-> ["+" |-> FixityInfo (Just InfixN) 8 8]
        ]
      popularity =
        [ "A" |-> 555,
          "B" |-> 2,
          "C" |-> 11103
        ]
      cabalDependencies = ["B", "C"]
      threshold = 0.4
      result =
        [ "+" |-> FixityInfo (Just InfixN) 6 8,
          "-" |-> FixityInfo (Just InfixL) 4 4
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

check7 :: Spec
check7 = it "whitelists declarations from base even when it is not listed in cabal dependencies" $ do
  let operators =
        [ "base" |-> ["+" |-> FixityInfo (Just InfixR) 4 4, "-" |-> FixityInfo (Just InfixR) 2 2],
          "B" |-> ["+" |-> FixityInfo (Just InfixN) 6 6, "-" |-> FixityInfo (Just InfixL) 4 4],
          "C" |-> ["+" |-> FixityInfo (Just InfixN) 8 8, "|>" |-> FixityInfo (Just InfixN) 1 1]
        ]
      popularity =
        [ "base" |-> 0,
          "B" |-> 2,
          "C" |-> 11103
        ]
      cabalDependencies = ["B", "C"]
      threshold = 0.4
      result =
        [ "+" |-> FixityInfo (Just InfixR) 4 4,
          "-" |-> FixityInfo (Just InfixR) 2 2,
          "|>" |-> FixityInfo (Just InfixN) 1 1
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

check8 :: Spec
check8 = it "whitelists declarations from base when base is also listed in cabal dependencies" $ do
  let operators =
        [ "base" |-> ["+" |-> FixityInfo (Just InfixR) 4 4, "-" |-> FixityInfo (Just InfixR) 2 2],
          "B" |-> ["+" |-> FixityInfo (Just InfixN) 6 6, "?=" |-> FixityInfo (Just InfixL) 4 4],
          "C" |-> ["<|>" |-> FixityInfo (Just InfixN) 8 8, "?=" |-> FixityInfo (Just InfixN) 1 1]
        ]
      popularity =
        [ "base" |-> 0,
          "B" |-> 2,
          "C" |-> 11103
        ]
      cabalDependencies = ["base", "B"]
      threshold = 0.6
      result =
        [ "+" |-> FixityInfo (Just InfixR) 4 4,
          "-" |-> FixityInfo (Just InfixR) 2 2,
          "?=" |-> FixityInfo (Just InfixL) 4 4
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

-- cabal dependencies override unspecified packages
check9 :: Spec
check9 = it "gives higher priority to declarations from cabal dependencies than declarations from unspecified packages" $ do
  let operators =
        [ "base" |-> ["+" |-> FixityInfo (Just InfixR) 4 4, "-" |-> FixityInfo (Just InfixR) 2 2],
          "B" |-> ["+" |-> FixityInfo (Just InfixN) 6 6, "?=" |-> FixityInfo (Just InfixL) 4 4],
          "C" |-> ["<|>" |-> FixityInfo (Just InfixN) 8 8, "?=" |-> FixityInfo (Just InfixN) 1 1]
        ]
      popularity =
        [ "base" |-> 0,
          "B" |-> 2,
          "C" |-> 11103
        ]
      cabalDependencies = ["base", "B"]
      threshold = 0.6
      result =
        [ "?=" |-> FixityInfo (Just InfixL) 4 4,
          "<|>" |-> FixityInfo (Just InfixN) 8 8
        ]
  checkFixityMap' operators popularity cabalDependencies threshold result

-- complex case mixing all these tests

-- #########################################################################
-- tests with buildFixityMap (actual Hackage database)
-- #########################################################################

-- ":" (R, 5, 5)

-- "<|>" (L, 3, 3)

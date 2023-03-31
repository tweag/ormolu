{-# LANGUAGE OverloadedStrings #-}

module Ormolu.HackageInfoSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Distribution.Types.PackageName (PackageName)
import Ormolu.Fixity
import Test.Hspec

-- | Build a fixity map using the Hackage/Hoogle database, and the boot
-- package list, and then check the fixity of the specified subset of
-- operators.
checkFixityMap ::
  -- | List of dependencies
  [PackageName] ->
  -- | Threshold to choose the conflict resolution strategy
  Float ->
  -- | Associative list representing a subset of the resulting fixity map
  -- that should be checked.
  [(OpName, FixityInfo)] ->
  Expectation
checkFixityMap
  dependencies
  threshold
  expectedResult =
    actualResult `shouldBe` expectedResult
    where
      actualResult =
        mapMaybe
          (\(k, _) -> (k,) <$> lookupFixity k resultMap)
          expectedResult
      resultMap =
        buildFixityMap'
          packageToOps
          packageToPopularity
          bootPackages
          threshold
          (Set.fromList dependencies)

-- | Build a fixity map from a custom package database, and then check the
-- fixity of the specified subset of operators.
checkFixityMap' ::
  -- | Associative list for packageToOps:
  -- package name -map-> (operator -map-> fixity)
  [(PackageName, [(OpName, FixityInfo)])] ->
  -- | Associative list for packageToPopularity:
  -- package name -map-> download count
  [(PackageName, Int)] ->
  -- | List of packages that should have a higher priority than
  -- unspecified packages (boot packages)
  [PackageName] ->
  -- | List of dependencies
  [PackageName] ->
  -- | Threshold to choose the conflict resolution strategy
  Float ->
  -- | Associative list representing a subset of the resulting fixity map
  -- that should be checked.
  [(OpName, FixityInfo)] ->
  Expectation
checkFixityMap'
  lPackageToOps
  lPackageToPopularity
  highPrioPackages
  dependencies
  threshold
  expectedResult =
    actualResult `shouldBe` expectedResult
    where
      actualResult =
        mapMaybe
          (\(k, _) -> (k,) <$> lookupFixity k resultMap)
          expectedResult
      resultMap =
        buildFixityMap'
          lPackageToOps'
          lPackageToPopularity'
          (Set.fromList highPrioPackages)
          threshold
          (Set.fromList dependencies)
      lPackageToOps' =
        Map.map Map.fromList $
          Map.fromList lPackageToOps
      lPackageToPopularity' = Map.fromList lPackageToPopularity

spec :: Spec
spec = do
  it
    "correctly merges fixities when a conflict appears in unspecified \
    \packages, with max(pop) < threshold"
    $ do
      let operators =
            [ ("A", [("+", FixityInfo (Just InfixL) 4 4)]),
              ("B", [("+", FixityInfo (Just InfixR) 6 6)])
            ]
          popularity =
            [ ("A", 3),
              ("B", 5)
            ]
          dependencies = []
          higherPriorityPackages = []
          threshold = 0.9
          result =
            [ ("+", FixityInfo Nothing 4 6)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "keeps only the most popular declaration when a conflict appears in \
    \unspecified packages, with max(pop) >= threshold"
    $ do
      let operators =
            [ ("A", [("+", FixityInfo (Just InfixL) 4 4)]),
              ("B", [("+", FixityInfo (Just InfixR) 6 6)])
            ]
          popularity =
            [ ("A", 5),
              ("B", 103)
            ]
          dependencies = []
          higherPriorityPackages = []
          threshold = 0.9
          result =
            [ ("+", FixityInfo (Just InfixR) 6 6)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "merges the ex-aequo most popular declaration when a conflict appears \
    \in unspecified packages, with max(pop) >= threshold"
    $ do
      let operators =
            [ ("A", [("+", FixityInfo (Just InfixL) 4 4)]),
              ("B", [("+", FixityInfo (Just InfixR) 6 6)]),
              ("C", [("+", FixityInfo (Just InfixR) 8 8)])
            ]
          popularity =
            [ ("A", 5),
              ("B", 103),
              ("C", 103)
            ]
          dependencies = []
          higherPriorityPackages = []
          threshold = 0.4
          result =
            [ ("+", FixityInfo (Just InfixR) 6 8)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "keeps only the most popular declaration when a conflict appears in \
    \unspecified packages, threshold == 0"
    $ do
      let operators =
            [ ("A", [("+", FixityInfo (Just InfixL) 4 4)]),
              ("B", [("+", FixityInfo (Just InfixR) 6 6)])
            ]
          popularity =
            [ ("A", 5),
              ("B", 103)
            ]
          dependencies = []
          higherPriorityPackages = []
          threshold = 0.0
          result =
            [ ("+", FixityInfo (Just InfixR) 6 6)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "merges all declarations when a conflict appears in unspecified \
    \packages, threshold > 1"
    $ do
      let operators =
            [ ("A", [("+", FixityInfo (Just InfixN) 4 4)]),
              ("B", [("+", FixityInfo (Just InfixN) 6 6)]),
              ("C", [("+", FixityInfo (Just InfixN) 8 8)])
            ]
          popularity =
            [ ("A", 0),
              ("B", 0),
              ("C", 11103)
            ]
          dependencies = []
          higherPriorityPackages = []
          threshold = 10.0
          result =
            [ ("+", FixityInfo (Just InfixN) 4 8)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "merges all declarations when a conflict appears in cabal \
    \dependencies"
    $ do
      let operators =
            [ ( "A",
                [ ("+", FixityInfo (Just InfixR) 4 4),
                  ("-", FixityInfo (Just InfixR) 2 2)
                ]
              ),
              ( "B",
                [ ("+", FixityInfo (Just InfixN) 6 6),
                  ("-", FixityInfo (Just InfixL) 4 4)
                ]
              ),
              ("C", [("+", FixityInfo (Just InfixN) 8 8)])
            ]
          popularity =
            [ ("A", 0),
              ("B", 0),
              ("C", 11103)
            ]
          dependencies = ["B", "C"]
          higherPriorityPackages = []
          threshold = 0.4
          result =
            [ ("+", FixityInfo (Just InfixN) 6 8),
              ("-", FixityInfo (Just InfixL) 4 4)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "merges all declarations when a conflict appears in higher-priority \
    \packages"
    $ do
      let operators =
            [ ( "A",
                [ ("+", FixityInfo (Just InfixR) 4 4),
                  ("-", FixityInfo (Just InfixR) 2 2)
                ]
              ),
              ( "B",
                [ ("+", FixityInfo (Just InfixN) 6 6),
                  ("-", FixityInfo (Just InfixL) 4 4)
                ]
              ),
              ("C", [("+", FixityInfo (Just InfixN) 8 8)])
            ]
          popularity =
            [ ("A", 0),
              ("B", 0),
              ("C", 11103)
            ]
          dependencies = []
          higherPriorityPackages = ["B", "C"]
          threshold = 0.4
          result =
            [ ("+", FixityInfo (Just InfixN) 6 8),
              ("-", FixityInfo (Just InfixL) 4 4)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "whitelists declarations from base even when it is not listed in \
    \cabal dependencies"
    $ do
      let operators =
            [ ( "base",
                [ ("+", FixityInfo (Just InfixR) 4 4),
                  ("-", FixityInfo (Just InfixR) 2 2)
                ]
              ),
              ( "B",
                [ ("+", FixityInfo (Just InfixN) 6 6),
                  ("-", FixityInfo (Just InfixL) 4 4)
                ]
              ),
              ( "C",
                [ ("+", FixityInfo (Just InfixN) 8 8),
                  ("|>", FixityInfo (Just InfixN) 1 1)
                ]
              )
            ]
          popularity =
            [ ("base", 0),
              ("B", 2),
              ("C", 11103)
            ]
          dependencies = ["B", "C"]
          higherPriorityPackages = []
          threshold = 0.4
          result =
            [ ("+", FixityInfo (Just InfixR) 4 4),
              ("-", FixityInfo (Just InfixR) 2 2),
              ("|>", FixityInfo (Just InfixN) 1 1)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "whitelists declarations from base when base is also listed in cabal \
    \dependencies"
    $ do
      let operators =
            [ ( "base",
                [ ("+", FixityInfo (Just InfixR) 4 4),
                  ("-", FixityInfo (Just InfixR) 2 2)
                ]
              ),
              ( "B",
                [ ("+", FixityInfo (Just InfixN) 6 6),
                  ("?=", FixityInfo (Just InfixL) 4 4)
                ]
              ),
              ( "C",
                [ ("<|>", FixityInfo (Just InfixN) 8 8),
                  ("?=", FixityInfo (Just InfixN) 1 1)
                ]
              )
            ]
          popularity =
            [ ("base", 0),
              ("B", 2),
              ("C", 11103)
            ]
          dependencies = ["base", "B"]
          higherPriorityPackages = []
          threshold = 0.6
          result =
            [ ("+", FixityInfo (Just InfixR) 4 4),
              ("-", FixityInfo (Just InfixR) 2 2),
              ("?=", FixityInfo (Just InfixL) 4 4)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "gives higher priority to declarations from cabal dependencies than \
    \declarations from both higher-priority & unspecified packages"
    $ do
      let operators =
            [ ( "base",
                [ ("+", FixityInfo (Just InfixR) 4 4),
                  ("-", FixityInfo (Just InfixR) 2 2)
                ]
              ),
              ( "B",
                [ ("+", FixityInfo (Just InfixN) 6 6),
                  ("?=", FixityInfo (Just InfixL) 4 4)
                ]
              ),
              ( "C",
                [ ("<|>", FixityInfo (Just InfixN) 8 8),
                  ("?=", FixityInfo (Just InfixN) 1 1)
                ]
              ),
              ("D", [("+", FixityInfo (Just InfixR) 2 2)])
            ]
          popularity =
            [ ("base", 0),
              ("B", 2),
              ("C", 11103)
            ]
          dependencies = ["base", "B"]
          higherPriorityPackages = ["D"]
          threshold = 0.6
          result =
            [ ("?=", FixityInfo (Just InfixL) 4 4),
              ("<|>", FixityInfo (Just InfixN) 8 8)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it
    "gives higher priority to declarations from higher-priority packages \
    \than declarations from unspecified packages"
    $ do
      let operators =
            [ ( "base",
                [ ("+", FixityInfo (Just InfixR) 4 4),
                  ("-", FixityInfo (Just InfixR) 2 2)
                ]
              ),
              ( "B",
                [ ("+", FixityInfo (Just InfixN) 6 6),
                  ("?=", FixityInfo (Just InfixL) 4 4)
                ]
              ),
              ( "C",
                [ ("<|>", FixityInfo (Just InfixN) 8 8),
                  ("?=", FixityInfo (Just InfixN) 1 1)
                ]
              ),
              ("D", [("+", FixityInfo (Just InfixR) 2 2)])
            ]
          popularity =
            [ ("base", 0),
              ("B", 2),
              ("C", 11103)
            ]
          dependencies = []
          higherPriorityPackages = ["B"]
          threshold = 0.6
          result =
            [ ("+", FixityInfo (Just InfixR) 4 4),
              ("?=", FixityInfo (Just InfixL) 4 4),
              ("<|>", FixityInfo (Just InfixN) 8 8)
            ]
      checkFixityMap'
        operators
        popularity
        higherPriorityPackages
        dependencies
        threshold
        result

  it "gives the correct fixity info for ':' (from base)" $ do
    let dependencies = []
        threshold = 0.6
        result =
          [ (":", FixityInfo (Just InfixR) 5 5)
          ]
    checkFixityMap dependencies threshold result

  it
    "gives the base's fixity info for '<|>', even when a dependency has a \
    \conflicting declaration for it"
    $ do
      let dependencies = ["pandoc"]
          threshold = 0.9
          result =
            [ ("<|>", FixityInfo (Just InfixL) 3 3)
            ]
      checkFixityMap dependencies threshold result

  it
    "gives the containers's fixity info for ':>' (because 'containers' is \
    \a higher-priority package), even though max(pop) < threshold for \
    \this operator)"
    $ do
      let dependencies = []
          threshold = 0.9
          result =
            [ (":>", FixityInfo (Just InfixL) 5 5)
            ]
      checkFixityMap dependencies threshold result

  it
    "gives the servant's fixity info for ':>' once servant is added as a \
    \dependency (although ':>' is also defined in 'containers', a \
    \higher-priority package)"
    $ do
      let dependencies = ["servant"]
          threshold = 0.9
          result =
            [ (":>", FixityInfo (Just InfixR) 4 4)
            ]
      checkFixityMap dependencies threshold result

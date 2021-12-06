{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Definitons for fixity analysis.
module Ormolu.Fixity
  ( FixityDirection (..),
    FixityInfo (..),
    FixityMap,
    HackageInfo (..),
    defaultFixityInfo,
    defaultFixityMap,
    buildFixityMap,
    buildFixityMap',
    bootPackages,
    packageToOps,
    packageToPopularity,
    defaultStrategyThreshold,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as A
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import qualified Language.Haskell.TH.Syntax as TH
import Ormolu.Fixity.Types

packageToOps :: Map String FixityMap
packageToPopularity :: Map String Int
HackageInfo packageToOps packageToPopularity =
  $( do
       let path = "extract-hackage-info/hackage-info.json"
       info <- liftIO $ either fail pure =<< A.eitherDecodeFileStrict' path
       TH.lift (info :: HackageInfo)
   )

-- | List of packages shipped with GHC, for which the download count from
-- Hackage does not reflect their high popularity.
-- See https://github.com/tweag/ormolu/pull/830#issuecomment-986609572.
-- "base" is not is this list, because it is already whitelisted
-- by buildFixityMap'.
bootPackages :: [String]
bootPackages =
  [ "array",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "exceptions",
    "filepath",
    "ghc-binary",
    "mtl",
    "parsec",
    "process",
    "stm",
    "template-haskell",
    "terminfo",
    "text",
    "time",
    "transformers",
    "unix",
    "Win32"
  ]

-- | The default value for the popularity ratio threshold, after which one
-- very popular definition from packageToOps will completely rule out
-- conflicting definitions instead of being merged with them.
defaultStrategyThreshold :: Float
defaultStrategyThreshold = 0.9

-- | The default fixity map, using the default value for the popularity
-- ratio threshold, and an empty list of dependencies.
defaultFixityMap :: FixityMap
defaultFixityMap = buildFixityMap [] defaultStrategyThreshold

-- | Build a fixity map using the given popularity threshold and a list of
-- cabal dependencies. Dependencies from the list have higher priority than
-- other packages.
buildFixityMap ::
  -- | Explicitely known dependencies
  [String] ->
  -- | Popularity ratio threshold, after which a very popular package will
  -- completely rule out conflicting definitions coming from other packages
  -- instead of being merged with them
  Float ->
  -- | Resulting map
  FixityMap
buildFixityMap = buildFixityMap' packageToOps packageToPopularity bootPackages

-- | Build a fixity map using the given popularity threshold and a list of
-- cabal dependencies. Dependencies from the list have higher priority than
-- other packages.
-- This specific version of the function allows the user to specify
-- the package databases (package -> fixityMap and package -> popularity)
-- used to build the final fixity map (op -> fixity).
buildFixityMap' ::
  -- | Map package -> fixity map for operators defined in this package
  Map String FixityMap ->
  -- | Map package -> popularity
  Map String Int ->
  -- | Higher priority packages
  [String] ->
  -- | Explicitely known dependencies
  [String] ->
  -- | Popularity ratio threshold, after which a very popular package will
  -- completely rule out conflicting definitions coming from other packages
  -- instead of being merged with them
  Float ->
  -- | Resulting map
  FixityMap
buildFixityMap'
  operatorMap
  popularityMap
  higherPriorityPackages
  dependencies
  strategyThreshold =
    -- Map.union is left biaised
    Map.union baseFixityMap $
      Map.union cabalFixityMap $
        Map.union higherPriorityFixityMap remainingFixityMap
    where
      baseFixityMap =
        Map.insert ":" colonFixityInfo $
          fromMaybe Map.empty $
            Map.lookup "base" operatorMap
      cabalFixityMap =
        mergeAll (buildPackageFixityMap <$> dependencies)
      higherPriorityFixityMap =
        mergeAll (buildPackageFixityMap <$> higherPriorityPackages)
      remainingFixityMap =
        mergeFixityMaps
          popularityMap
          strategyThreshold
          (buildPackageFixityMap <$> remainingPackages)
      remainingPackages =
        Map.keys operatorMap
          `difference` (dependencies ++ higherPriorityPackages)
      buildPackageFixityMap packageName =
        ( packageName,
          fromMaybe Map.empty $
            Map.lookup packageName operatorMap
        )
      difference l1 l2 =
        Set.toList $
          Set.fromList l1 `Set.difference` Set.fromList l2
      mergeAll = mergeFixityMaps Map.empty 10.0

-- | Merge a list of individual fixity maps, coming from different packages.
-- Package popularities and the given threshold are used to choose between
-- the "keep best only" (>= threshold) and "merge all" (< threshold)
-- strategies when conflicting definitions are encountered for an operator.
mergeFixityMaps ::
  -- | Map package -> popularity
  Map String Int ->
  -- | Popularity ratio threshold
  Float ->
  -- | List of (package name, package fixity map) to merge
  [(String, FixityMap)] ->
  -- | Resulting fixity map
  FixityMap
mergeFixityMaps popularityMap threshold packageMaps =
  Map.map
    (useThreshold threshold . NE.fromList . Map.toList)
    scoredMap
  where
    scoredMap = Map.map getScores opFixityMap
    -- when we encounter a duplicate key (op1) in the unionsWith operation,
    -- we have
    --   op1 -map-> {definitions1 -map-> originPackages}
    --   op1 -map-> {definitions2 -map-> originPackages}
    -- so we merge the keys (which have the type:
    -- Map FixityInfo (NonEmpty String))
    -- using 'Map.unionWith (<>)', to "concatenate" the list of
    -- definitions for this operator, and to also "concatenate" origin
    -- packages if a same definition is found in both maps
    opFixityMap =
      Map.unionsWith
        (Map.unionWith (<>))
        (opFixityMapFrom <$> packageMaps)
    useThreshold ::
      -- Threshold
      Float ->
      -- List of conflicting (definition, score) for a given operator
      NonEmpty (FixityInfo, Int) ->
      -- Resulting fixity, using the specified threshold to choose between
      -- strategy "keep best only" and "merge all"
      FixityInfo
    useThreshold t fixScores =
      if toFloat maxScore / toFloat sumScores >= t
        then sconcat . fmap fst $ maxs -- merge potential ex-aequo winners
        else sconcat . fmap fst $ fixScores
      where
        toFloat x = fromIntegral x :: Float
        maxs = maxWith snd fixScores
        maxScore = snd $ NE.head maxs
        sumScores = foldl' (+) 0 (snd <$> fixScores)
    getScores ::
      -- Map for a given operator associating each of its conflicting
      -- definitions with the packages that define it
      Map FixityInfo (NonEmpty String) ->
      -- Map for a given operator associating each of its conflicting
      -- definitions with their score (= sum of the popularity of the
      -- packages that define it)
      Map FixityInfo Int
    getScores =
      Map.map
        (sum . fmap (fromMaybe 0 . flip Map.lookup popularityMap))
    opFixityMapFrom ::
      -- (packageName, package fixity map)
      (String, FixityMap) ->
      -- Map associating each operator of the package with a
      -- {map for a given operator associating each of its definitions with
      -- the list of packages that define it}
      -- (this list can only be == [packageName] in the context of this
      -- function)
      Map String (Map FixityInfo (NonEmpty String))
    opFixityMapFrom (packageName, opsMap) =
      Map.map
        (flip Map.singleton (packageName :| []))
        opsMap
    maxWith :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
    maxWith f xs = snd $ foldl' comp (f h, h :| []) t
      where
        h :| t = xs
        comp (fMax, maxs) x =
          let fX = f x
           in if
                  | fMax < fX -> (fX, x :| [])
                  | fMax == fX -> (fMax, NE.cons x maxs)
                  | otherwise -> (fMax, maxs)

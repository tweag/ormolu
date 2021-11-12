{-# LANGUAGE RecordWildCards #-}
module Ormolu.Printer.FixityConfig where

import Ormolu.Printer.FixityInfo
import Ormolu.Printer.HoogleHackageOperatorInfo (packageToPopularity, packageToOps)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

data FixityConfig = FixityConfig
  { fcCustomFixityMap :: Map String FixityInfo,
    -- do not forget to special case base here
    -- if base not in fcPackagesManualConfig:
    --     fcPackagesManualConfig = base : fcPackagesManualConfig
    -- ordering matters for this field
    fcPackagesManualConfig :: [(String, PackageConfig)],
    fcDetectCabalDependencies :: Bool,
    -- Nothing means do not use hoogle
    fcUseHoogleForUnspecifiedPackages :: Maybe ConflictStrategy
  }

defaultFixityConfig :: FixityConfig
defaultFixityConfig = FixityConfig
  { fcCustomFixityMap = Map.empty,
    fcPackagesManualConfig = [],
    fcDetectCabalDependencies = True,
    fcUseHoogleForUnspecifiedPackages = Just $ UseThreshold 0.9
  }

data PackageConfig =
  AllExcept [String]
  | NoneExcept [String]

data ConflictStrategy =
  KeepBest
  | MergeAll
  | UseThreshold Float

buildFixityMap ::
  -- | Cabal dependencies
  [String] ->
  FixityConfig ->
  Map String FixityInfo
buildFixityMap cabalDependencies FixityConfig{..} =
  addOverride fcCustomFixityMap postManualConfigFixityMap where
    postManualConfigFixityMap = foldr addOverride postCabalFixityMap (buildPackageFixityMap <$> fcPackagesManualConfig')
    fcPackagesManualConfig' = if any ((== "base") . fst) fcPackagesManualConfig
      then fcPackagesManualConfig
      else ("base", AllExcept []) : fcPackagesManualConfig
    postCabalFixityMap = addOverride cabalFixityMap hoogleFixityMap
    cabalFixityMap = if fcDetectCabalDependencies then mergeFixityMaps KeepBest (buildPackageFixityMap <$> allOpsFromEach cabalDependenciesNotManuallyConfigured) else Map.empty
    cabalDependenciesNotManuallyConfigured = Set.toList $ (Set.fromList $ cabalDependencies) `Set.difference` (Set.fromList $ fst <$> fcPackagesManualConfig')
    hoogleFixityMap = case fcUseHoogleForUnspecifiedPackages of
      Nothing -> Map.empty
      Just conflictStrategy -> mergeFixityMaps conflictStrategy (buildPackageFixityMap <$> allOpsFromEach unspecifiedPackages)
    specifiedPackages = (fst <$> fcPackagesManualConfig') ++ (if fcDetectCabalDependencies then cabalDependenciesNotManuallyConfigured else [])
    unspecifiedPackages = Set.toList $ (Set.fromList $ Map.keys packageToOps) `Set.difference` (Set.fromList specifiedPackages)
    allOpsFromEach xs = zip xs (repeat $ AllExcept [])
    addOverride = Map.union  -- Map.union is left biaised, so the left argument will always

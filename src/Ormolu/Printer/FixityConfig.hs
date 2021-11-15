{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Printer.FixityConfig where

import qualified Data.Either as Either
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import Ormolu.Printer.FixityInfo
import Ormolu.Printer.HoogleHackageOperatorInfo (packageToOps, packageToPopularity)

defaultFixityMap :: HashMap String FixityInfo
defaultFixityMap = Either.fromRight HashMap.empty $ buildFixityMap [] defaultFixityConfig

data FixityConfig = FixityConfig
  { fcCustomFixityMap :: HashMap String FixityInfo,
    -- ordering matters for this field
    fcPackagesManualConfig :: [(String, PackageConfig)],
    fcDetectCabalDependencies :: Bool,
    -- Nothing means do not use hoogle
    fcUseHoogleForUnspecifiedPackages :: Maybe ConflictStrategy
  }

defaultFixityConfig :: FixityConfig
defaultFixityConfig =
  FixityConfig
    { fcCustomFixityMap = HashMap.empty,
      fcPackagesManualConfig = [],
      fcDetectCabalDependencies = True,
      fcUseHoogleForUnspecifiedPackages = Just $ UseThreshold 0.9
    }

data PackageConfig
  = AllExcept [String]
  | NoneExcept [String]

data ConflictStrategy
  = KeepBest
  | MergeAll
  | UseThreshold Float

-- | Left = list of packages not found, right = all packages were found
checkPackagesSpecifiedManually :: FixityConfig -> Either [String] ()
checkPackagesSpecifiedManually FixityConfig {fcPackagesManualConfig} =
  case filter (not . flip HashMap.member packageToOps) (fst <$> fcPackagesManualConfig) of
    [] -> Right ()
    xs -> Left xs

buildFixityMap ::
  -- | Cabal dependencies
  [String] ->
  FixityConfig ->
  Either [String] (HashMap String FixityInfo)
buildFixityMap cabalDependencies config@FixityConfig {..} = do
  checkPackagesSpecifiedManually config
  return $ addOverride fcCustomFixityMap postManualConfigFixityMap
  where
    postManualConfigFixityMap = foldr addOverride postCabalFixityMap (snd . buildPackageFixityMap <$> fcPackagesManualConfig')
    fcPackagesManualConfig' =
      if any ((== "base") . fst) fcPackagesManualConfig
        then fcPackagesManualConfig
        else ("base", AllExcept []) : fcPackagesManualConfig
    postCabalFixityMap = addOverride cabalFixityMap hoogleFixityMap
    cabalFixityMap = if fcDetectCabalDependencies then mergeFixityMaps KeepBest (buildPackageFixityMap <$> allOpsFromEach cabalDependenciesNotManuallyConfigured) else HashMap.empty
    cabalDependenciesNotManuallyConfigured = Set.toList $ (Set.fromList $ cabalDependencies) `Set.difference` (Set.fromList $ fst <$> fcPackagesManualConfig')
    hoogleFixityMap = case fcUseHoogleForUnspecifiedPackages of
      Nothing -> HashMap.empty
      Just conflictStrategy -> mergeFixityMaps conflictStrategy (buildPackageFixityMap <$> allOpsFromEach unspecifiedPackages)
    specifiedPackages = (fst <$> fcPackagesManualConfig') ++ (if fcDetectCabalDependencies then cabalDependenciesNotManuallyConfigured else [])
    unspecifiedPackages = Set.toList $ (Set.fromList $ HashMap.keys packageToOps) `Set.difference` (Set.fromList specifiedPackages)
    allOpsFromEach xs = zip xs (repeat $ AllExcept [])
    addOverride = HashMap.union -- HashMap.union is left biaised

mergeFixityMaps :: ConflictStrategy -> [(String, HashMap String FixityInfo)] -> HashMap String FixityInfo
mergeFixityMaps strategy packagesMaps =
  HashMap.map
    ( ( case strategy of
          KeepBest -> keepBest
          MergeAll -> mergeAll
          UseThreshold t -> useThreshold t
      )
        . NE.fromList
        . HashMap.toList
    )
    scoredMap
  where
    keepBest :: NonEmpty (FixityInfo, Int) -> FixityInfo
    keepBest = sconcat . fmap fst . maxWith snd
    mergeAll :: NonEmpty (FixityInfo, Int) -> FixityInfo
    mergeAll = sconcat . fmap fst
    useThreshold :: Float -> NonEmpty (FixityInfo, Int) -> FixityInfo
    useThreshold t fixScores =
      if (fromIntegral maxScore :: Float) / (fromIntegral sumScores :: Float) >= t
        then keepBest fixScores
        else mergeAll fixScores
      where
        maxs = maxWith snd fixScores
        maxScore = snd $ NE.head maxs
        sumScores = foldl' (+) 0 (snd <$> fixScores)
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
    scoredMap = HashMap.map getScores opFixityMap
    getScores :: HashMap FixityInfo (NonEmpty String) -> HashMap FixityInfo Int
    getScores = HashMap.map (sum . fmap (fromMaybe 0 . flip HashMap.lookup packageToPopularity))
    opFixityMap = unionsWith mergeOpFixityMaps (opFixityMapFrom <$> packagesMaps)
    unionsWith :: (Eq k, Hashable k) => (v -> v -> v) -> [HashMap k v] -> HashMap k v
    unionsWith f = \case
      [] -> HashMap.empty
      m : ms -> foldl' (HashMap.unionWith f) m ms
    mergeOpFixityMaps :: HashMap FixityInfo (NonEmpty String) -> HashMap FixityInfo (NonEmpty String) -> HashMap FixityInfo (NonEmpty String)
    mergeOpFixityMaps = HashMap.unionWith (<>)
    opFixityMapFrom :: (String, HashMap String FixityInfo) -> HashMap String (HashMap FixityInfo (NonEmpty String))
    opFixityMapFrom (packageName, opsMap) = HashMap.map (flip HashMap.singleton (packageName :| [])) opsMap

buildPackageFixityMap :: (String, PackageConfig) -> (String, HashMap String FixityInfo)
buildPackageFixityMap (packageName, config) =
  ( packageName,
    HashMap.filterWithKey
      ( case config of
          AllExcept xs -> \k _ -> k `notElem` xs
          NoneExcept xs -> \k _ -> k `elem` xs
      )
      $ fromMaybe HashMap.empty $
        HashMap.lookup packageName packageToOps
  )

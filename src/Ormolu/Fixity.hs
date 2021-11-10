{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
-- See https://github.com/haskell/haskell-language-server/issues/1841#issuecomment-843378909 if you encounter an issue with HLS
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ormolu.Fixity
  ( FixityInfo (..),
    FixityMap,
    HoogleHackageInfo (..),
    defaultFixityInfo,
    defaultFixityMap,
    buildFixityMap,
    defaultStrategyThreshold,
  )
where

import Data.Aeson (FromJSON, ToJSON, decodeStrict)
import qualified Data.ByteString
import Data.FileEmbed (embedFile)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Types.Fixity (FixityDirection (..))

deriving instance Generic FixityDirection

deriving instance Show FixityDirection

instance Hashable FixityDirection

instance FromJSON FixityDirection

instance ToJSON FixityDirection

type FixityMap = HashMap String FixityInfo

data HoogleHackageInfo = HoogleHackageInfo
  { hPackageToOps :: HashMap String FixityMap,
    hPackageToPopularity :: HashMap String Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON HoogleHackageInfo

instance ToJSON HoogleHackageInfo

hoogleHackageInfoFile :: Data.ByteString.ByteString
hoogleHackageInfoFile = $(embedFile "extract-hoogle-hackage-info/hoogle-hackage-info.json")

packageToOps :: HashMap String FixityMap
packageToPopularity :: HashMap String Int
HoogleHackageInfo {hPackageToOps = packageToOps, hPackageToPopularity = packageToPopularity} = fromJust . decodeStrict $ hoogleHackageInfoFile

-- | Gives fixity information (direction and precedence level) about an infix operator, but takes the uncertainty that can arise from conflicting definitions into account.
data FixityInfo = FixityInfo
  { -- | Fixity direction (InfixL, InfixR, or InfixN (not associative)), if it is known
    fixDir :: Maybe FixityDirection,
    -- | Minimum precedence level found in the (maybe conflicting) definitions for the operator (inclusive)
    fixMinPrec :: Int,
    -- | Maximum precedence level found in the (maybe conflicting) definitions for the operator (inclusive)
    fixMaxPrec :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON FixityInfo

instance ToJSON FixityInfo

-- | Corresponds to the lowest level of information we can get about an operator (no information for fixity direction, and a precedence between 0 and 9).
defaultFixityInfo :: FixityInfo
defaultFixityInfo =
  FixityInfo
    { fixDir = Nothing,
      fixMinPrec = 0,
      fixMaxPrec = 9
    }

-- | Gives the ability to merge two (maybe conflicting) definitions for an operator, keeping the higher level of compatible information from both.
instance Semigroup FixityInfo where
  FixityInfo {fixDir = dir1, fixMinPrec = min1, fixMaxPrec = max1}
    <> FixityInfo {fixDir = dir2, fixMinPrec = min2, fixMaxPrec = max2} =
      FixityInfo
        { fixDir = dir',
          fixMinPrec = min min1 min2,
          fixMaxPrec = max max1 max2
        }
      where
        dir' = case (dir1, dir2) of
          (Just a, Just b) | a == b -> Just a
          _ -> Nothing

instance Hashable FixityInfo

defaultStrategyThreshold :: Float
defaultStrategyThreshold = 0.9

defaultFixityMap :: FixityMap
defaultFixityMap = buildFixityMap [] defaultStrategyThreshold

buildFixityMap :: [String] -> Float -> FixityMap
buildFixityMap cabalDependencies strategyThreshold =
  HashMap.union baseFixityMap (HashMap.union cabalFixityMap hoogleFixityMap) -- HashMap.union is left biaised
  where
    baseFixityMap = fromMaybe HashMap.empty $ HashMap.lookup "base" packageToOps
    cabalFixityMap = mergeFixityMaps 1.0 (buildPackageFixityMap <$> cabalDependencies)
    hoogleFixityMap = mergeFixityMaps strategyThreshold (buildPackageFixityMap <$> notCabalDependencies)
    notCabalDependencies = HashMap.keys packageToOps `difference` cabalDependencies
    buildPackageFixityMap packageName =
      ( packageName,
        fromMaybe HashMap.empty $
          HashMap.lookup packageName packageToOps
      )
    difference l1 l2 = Set.toList $ Set.fromList l1 `Set.difference` Set.fromList l2

mergeFixityMaps :: Float -> [(String, FixityMap)] -> FixityMap
mergeFixityMaps threshold packageMaps =
  HashMap.map (useThreshold threshold . NE.fromList . HashMap.toList) scoredMap
  where
    scoredMap = HashMap.map getScores opFixityMap
    -- when we encounter a duplicate key (op1) in the unionsWith operation, we have
    --   op1 -map-> {definitions1 -map-> originPackages}
    --   op1 -map-> {definitions2 -map-> originPackages}
    -- so we merge the keys (which have the type: HashMap FixityInfo (NonEmpty String))
    -- using 'HashMap.unionWith (<>)', to "concatenate" the list of definitions
    -- for this operator, and to also "concatenate" origin packages if a same
    -- definition is found in both maps
    opFixityMap = unionsWith (HashMap.unionWith (<>)) (opFixityMapFrom <$> packageMaps)
    useThreshold ::
      -- Threshold
      Float ->
      -- List of conflicting (definition, score) for a given operator
      NonEmpty (FixityInfo, Int) ->
      -- Resulting fixity, using the specified threshold to choose between strategy "keep best" and "merge all"
      FixityInfo
    useThreshold t fixScores =
      if (fromIntegral maxScore :: Float) / (fromIntegral sumScores :: Float) >= t
        then sconcat . fmap fst $ maxs -- merge potential ex-aequo winners
        else sconcat . fmap fst $ fixScores
      where
        maxs = maxWith snd fixScores
        maxScore = snd $ NE.head maxs
        sumScores = foldl' (+) 0 (snd <$> fixScores)
    getScores ::
      -- Map for a given operator associating each of its conflicting definitions with the packages that define it
      HashMap FixityInfo (NonEmpty String) ->
      -- Map for a given operator associating each of its conflicting definitions with their score (= sum of the popularity of the packages that define it)
      HashMap FixityInfo Int
    getScores = HashMap.map (sum . fmap (fromMaybe 0 . flip HashMap.lookup packageToPopularity))
    opFixityMapFrom ::
      -- (packageName, package fixity map)
      (String, FixityMap) ->
      -- Map associating each operator of the package with a
      -- {map for a given operator associating each of its definitions with the list of packages that define it}
      -- (this list can only be == [packageName] in the context of this function)
      HashMap String (HashMap FixityInfo (NonEmpty String))
    opFixityMapFrom (packageName, opsMap) = HashMap.map (flip HashMap.singleton (packageName :| [])) opsMap
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
    -- Same as Map.unionsWith, but for HashMap
    unionsWith :: (Eq k, Hashable k) => (v -> v -> v) -> [HashMap k v] -> HashMap k v
    unionsWith f = \case
      [] -> HashMap.empty
      m : ms -> foldl' (HashMap.unionWith f) m ms

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Definitons for fixity analysis.
module Ormolu.Fixity
  ( FixityInfo (..),
    FixityMap,
    HackageInfo (..),
    defaultFixityInfo,
    defaultFixityMap,
    buildFixityMap,
    buildFixityMap',
    hPackageToOps,
    hPackageToPopularity,
    defaultStrategyThreshold,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.Types.Fixity (FixityDirection (..))

-- | Fixity information about an infix operator that takes the uncertainty
-- that can arise from conflicting definitions into account.
data FixityInfo = FixityInfo
  { -- | Fixity direction if it is known
    fiDirection :: Maybe FixityDirection,
    -- | Minimum precedence level found in the (maybe conflicting)
    -- definitions for the operator (inclusive)
    fiMinPrecedence :: Int,
    -- | Maximum precedence level found in the (maybe conflicting)
    -- definitions for the operator (inclusive)
    fiMaxPrecedence :: Int
  }
  deriving (Eq, Generic)

instance FromJSON FixityInfo where
  parseJSON = A.withObject "FixitiyInfo" $ \o ->
    FixityInfo
      <$> ( (o .:? "dir")
              >>= maybe (pure Nothing) (fmap Just . parseFixityDirection)
          )
      <*> o .: "min_prec"
      <*> o .: "max_prec"
    where
      parseFixityDirection = A.withText "FixityDirection" $ \case
        "InfixL" -> pure InfixL
        "InfixN" -> pure InfixN
        "InfixR" -> pure InfixR
        x -> fail (T.unpack x ++ " is not a fixity direction")

instance ToJSON FixityInfo where
  toJSON FixityInfo {..} =
    A.object
      [ "dir" .= (fixityDirectionToJSON <$> fiDirection),
        "min_prec" .= fiMinPrecedence,
        "max_prec" .= fiMaxPrecedence
      ]
    where
      fixityDirectionToJSON x =
        toJSON . T.pack . showFixityDirection $ x

showFixityDirection :: FixityDirection -> String
showFixityDirection = \case
  InfixN -> "InfixN"
  InfixR -> "InfixR"
  InfixL -> "InfixL"

instance Show FixityInfo where
  show FixityInfo {..} =
    "FixityInfo { "
      <> "fiDirection = "
      <> show (showFixityDirection <$> fiDirection)
      <> ", fiMinPrecedence = "
      <> show fiMinPrecedence
      <> ", fiMaxPrecedence = "
      <> show fiMaxPrecedence
      <> " }"

-- TODO try to get rid of hashable and by extension of the orphans

deriving instance Generic FixityDirection

instance Hashable FixityDirection

instance Hashable FixityInfo

-- | The lowest level of information we can have about an operator.
defaultFixityInfo :: FixityInfo
defaultFixityInfo =
  FixityInfo
    { fiDirection = Nothing,
      fiMinPrecedence = 0,
      fiMaxPrecedence = 9
    }

-- | Gives the ability to merge two (maybe conflicting) definitions for an
-- operator, keeping the higher level of compatible information from both.
instance Semigroup FixityInfo where
  FixityInfo {fiDirection = dir1, fiMinPrecedence = min1, fiMaxPrecedence = max1}
    <> FixityInfo {fiDirection = dir2, fiMinPrecedence = min2, fiMaxPrecedence = max2} =
      FixityInfo
        { fiDirection = dir',
          fiMinPrecedence = min min1 min2,
          fiMaxPrecedence = max max1 max2
        }
      where
        dir' = case (dir1, dir2) of
          (Just a, Just b) | a == b -> Just a
          _ -> Nothing

-- | Map from the operator name to its 'FixityInfo'.
type FixityMap = HashMap String FixityInfo

-- | The map of operators declared by each package and the popularity of
-- each package, if available.
data HackageInfo
  = HackageInfo
      (HashMap String FixityMap)
      (HashMap String Int)

instance FromJSON HackageInfo where
  parseJSON = A.withObject "HackageInfo" $ \o ->
    HackageInfo
      <$> o .: "operators"
      <*> o .: "popularity"

instance ToJSON HackageInfo where
  toJSON (HackageInfo operators popularity) =
    A.object
      [ "operators" .= operators,
        "popularity" .= popularity
      ]

-- | Embed the contents of JSON file containing 'HackageInfo' in a
-- serialized form.
hackageInfoFile :: ByteString
hackageInfoFile = $(embedFile "extract-hackage-info/hackage-info.json")

hPackageToOps :: HashMap String FixityMap
hPackageToPopularity :: HashMap String Int
HackageInfo hPackageToOps hPackageToPopularity =
  fromJust . A.decodeStrict $ hackageInfoFile

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
buildFixityMap = buildFixityMap' hPackageToOps hPackageToPopularity

-- | Build a fixity map using the given popularity threshold and a list of
-- cabal dependencies. Dependencies from the list have higher priority than
-- other packages.
-- This specific version of the function allows the user to specify
-- the package databases (package -> fixityMap and package -> popularity)
-- used to build the final fixity map (op -> fixity).
buildFixityMap' ::
  -- | Map package -> fixity map for operators defined in this package
  HashMap String FixityMap ->
  -- | Map package -> popularity
  HashMap String Int ->
  -- | Explicitely known dependencies
  [String] ->
  -- | Popularity ratio threshold, after which a very popular package will
  -- completely rule out conflicting definitions coming from other packages
  -- instead of being merged with them
  Float ->
  -- | Resulting map
  FixityMap
buildFixityMap'
  packageToOps
  packageToPopularity
  cabalDependencies
  strategyThreshold =
    -- HashMap.union is left biaised
    HashMap.union baseFixityMap (HashMap.union cabalFixityMap hoogleFixityMap)
    where
      baseFixityMap =
        fromMaybe HashMap.empty $
          HashMap.lookup "base" packageToOps
      cabalFixityMap =
        mergeFixityMaps
          packageToPopularity
          1.0 -- threshold = 1.0 means "merge all"
          (buildPackageFixityMap <$> cabalDependencies)
      hoogleFixityMap =
        mergeFixityMaps
          packageToPopularity
          strategyThreshold
          (buildPackageFixityMap <$> notCabalDependencies)
      notCabalDependencies =
        HashMap.keys packageToOps `difference` cabalDependencies
      buildPackageFixityMap packageName =
        ( packageName,
          fromMaybe HashMap.empty $
            HashMap.lookup packageName packageToOps
        )
      difference l1 l2 =
        Set.toList $
          Set.fromList l1 `Set.difference` Set.fromList l2

-- | Merge a list of individual fixity maps, coming from different packages.
-- Package popularities and the given threshold are used to choose between
-- the "keep best only" (>= threshold) and "merge all" (< threshold)
-- strategies when conflicting definitions are encountered for an operator.
mergeFixityMaps ::
  -- | Map package -> popularity
  HashMap String Int ->
  -- | Popularity ratio threshold
  Float ->
  -- | List of (package name, package fixity map) to merge
  [(String, FixityMap)] ->
  -- | Resulting fixity map
  FixityMap
mergeFixityMaps packageToPopularity threshold packageMaps =
  HashMap.map
    (useThreshold threshold . NE.fromList . HashMap.toList)
    scoredMap
  where
    scoredMap = HashMap.map getScores opFixityMap
    -- when we encounter a duplicate key (op1) in the unionsWith operation,
    -- we have
    --   op1 -map-> {definitions1 -map-> originPackages}
    --   op1 -map-> {definitions2 -map-> originPackages}
    -- so we merge the keys (which have the type:
    -- HashMap FixityInfo (NonEmpty String))
    -- using 'HashMap.unionWith (<>)', to "concatenate" the list of
    -- definitions for this operator, and to also "concatenate" origin
    -- packages if a same definition is found in both maps
    opFixityMap =
      unionsWith
        (HashMap.unionWith (<>))
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
      HashMap FixityInfo (NonEmpty String) ->
      -- Map for a given operator associating each of its conflicting
      -- definitions with their score (= sum of the popularity of the
      -- packages that define it)
      HashMap FixityInfo Int
    getScores =
      HashMap.map
        (sum . fmap (fromMaybe 0 . flip HashMap.lookup packageToPopularity))
    opFixityMapFrom ::
      -- (packageName, package fixity map)
      (String, FixityMap) ->
      -- Map associating each operator of the package with a
      -- {map for a given operator associating each of its definitions with
      -- the list of packages that define it}
      -- (this list can only be == [packageName] in the context of this
      -- function)
      HashMap String (HashMap FixityInfo (NonEmpty String))
    opFixityMapFrom (packageName, opsMap) =
      HashMap.map
        (flip HashMap.singleton (packageName :| []))
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
    -- Same as Map.unionsWith, but for HashMap
    unionsWith ::
      (Eq k, Hashable k) =>
      (v -> v -> v) ->
      [HashMap k v] ->
      HashMap k v
    unionsWith f = \case
      [] -> HashMap.empty
      m : ms -> foldl' (HashMap.unionWith f) m ms

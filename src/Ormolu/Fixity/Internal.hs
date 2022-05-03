{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Fixity.Internal
  ( FixityDirection (..),
    FixityInfo (..),
    defaultFixityInfo,
    colonFixityInfo,
    HackageInfo (..),
    FixityMap,
    LazyFixityMap (..),
    lookupFixity,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Instances.TH.Lift ()
import qualified Language.Haskell.TH.Syntax as TH

-- | Fixity direction.
data FixityDirection
  = InfixL
  | InfixR
  | InfixN
  deriving (Eq, Ord, Show, TH.Lift)

instance FromJSON FixityDirection where
  parseJSON = A.withText "FixityDirection" $ \case
    "InfixL" -> pure InfixL
    "InfixN" -> pure InfixN
    "InfixR" -> pure InfixR
    x -> fail (T.unpack x ++ " is not a fixity direction")

instance ToJSON FixityDirection where
  toJSON x =
    toJSON $ case x of
      InfixL -> "InfixL" :: Text
      InfixN -> "InfixN"
      InfixR -> "InfixR"

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
  deriving (Eq, Ord, Show, TH.Lift)

instance FromJSON FixityInfo where
  parseJSON = A.withObject "FixitiyInfo" $ \o ->
    FixityInfo
      <$> (o .:? "dir")
      <*> o .: "min_prec"
      <*> o .: "max_prec"

instance ToJSON FixityInfo where
  toJSON FixityInfo {..} =
    A.object
      [ "dir" .= fiDirection,
        "min_prec" .= fiMinPrecedence,
        "max_prec" .= fiMaxPrecedence
      ]

-- | The lowest level of information we can have about an operator.
defaultFixityInfo :: FixityInfo
defaultFixityInfo =
  FixityInfo
    { fiDirection = Nothing,
      fiMinPrecedence = 0,
      fiMaxPrecedence = 9
    }

-- | Fixity info of the built-in colon data constructor.
colonFixityInfo :: FixityInfo
colonFixityInfo =
  FixityInfo
    { fiDirection = Just InfixR,
      fiMinPrecedence = 5,
      fiMaxPrecedence = 5
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
type FixityMap = Map String FixityInfo

-- | A variant of 'FixityMap', represented as a lazy union of several
-- 'FixityMap's.
newtype LazyFixityMap = LazyFixityMap [FixityMap]
  deriving (Show)

-- | Lookup a 'FixityInfo' of an operator. This might have drastically
-- different performance depending on whether this is an "unusal"
-- operator.
lookupFixity :: String -> LazyFixityMap -> Maybe FixityInfo
lookupFixity op (LazyFixityMap maps) = asum (Map.lookup op <$> maps)

-- | The map of operators declared by each package and the popularity of
-- each package, if available.
data HackageInfo
  = HackageInfo
      (Map String FixityMap)
      -- ^ Map from package name to a map from operator name to its fixity
      (Map String Int)
      -- ^ Map from package name to its 30-days download count from Hackage
  deriving (TH.Lift)

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

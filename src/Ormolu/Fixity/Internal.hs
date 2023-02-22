{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Fixity.Internal
  ( OpName,
    pattern OpName,
    unOpName,
    occOpName,
    FixityDirection (..),
    FixityInfo (..),
    defaultFixityInfo,
    colonFixityInfo,
    HackageInfo (..),
    FixityMap,
    LazyFixityMap (..),
    lookupFixity,
  )
where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Distribution.Types.PackageName (PackageName)
import GHC.Data.FastString (fs_sbs)
import GHC.Generics (Generic)
import GHC.Types.Name (OccName (occNameFS))

-- | Fixity direction.
data FixityDirection
  = InfixL
  | InfixR
  | InfixN
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, NFData)

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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, NFData)

-- | The lowest level of information we can have about an operator.
defaultFixityInfo :: FixityInfo
defaultFixityInfo =
  FixityInfo
    { fiDirection = Just InfixL,
      fiMinPrecedence = 9,
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

-- | An operator name.
newtype OpName = MkOpName
  { -- | Invariant: UTF-8 encoded
    getOpName :: ShortByteString
  }
  deriving newtype (Eq, Ord, Binary, NFData)

-- | Convert an 'OpName' to 'Text'.
unOpName :: OpName -> Text
unOpName = T.decodeUtf8 . SBS.fromShort . getOpName

pattern OpName :: Text -> OpName
pattern OpName opName <- (unOpName -> opName)
  where
    OpName = MkOpName . SBS.toShort . T.encodeUtf8

{-# COMPLETE OpName #-}

-- | Convert an 'OccName to an 'OpName'.
occOpName :: OccName -> OpName
occOpName = MkOpName . fs_sbs . occNameFS

instance Show OpName where
  show = T.unpack . unOpName

instance IsString OpName where
  fromString = OpName . T.pack

-- | Map from the operator name to its 'FixityInfo'.
type FixityMap = Map OpName FixityInfo

-- | A variant of 'FixityMap', represented as a lazy union of several
-- 'FixityMap's.
newtype LazyFixityMap = LazyFixityMap [FixityMap]
  deriving (Show)

-- | Lookup a 'FixityInfo' of an operator. This might have drastically
-- different performance depending on whether this is an "unusual" operator.
lookupFixity :: OpName -> LazyFixityMap -> Maybe FixityInfo
lookupFixity op (LazyFixityMap maps) = asum (Map.lookup op <$> maps)

-- | The map of operators declared by each package and the popularity of
-- each package, if available.
data HackageInfo
  = HackageInfo
      -- | Map from package name to a map from operator name to its fixity
      (Map PackageName FixityMap)
      -- | Map from package name to its 30-days download count from Hackage
      (Map PackageName Int)
  deriving stock (Generic)
  deriving anyclass (Binary)

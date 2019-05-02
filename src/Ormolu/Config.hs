{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Configuration options used by the tool.

module Ormolu.Config
  ( Config (..)
  , defaultConfig
  , DynOption (..)
  , dynOptionToLocatedStr
  )
where

import Data.Yaml
import qualified SrcLoc as GHC

-- | Ormolu configuration.

data Config = Config
  { cfgDynOptions :: ![DynOption]
    -- ^ Dynamic options to pass to GHC parser
  , cfgUnsafe :: !Bool
    -- ^ Do formatting faster but without automatic detection of defects
  , cfgDebug :: !Bool
    -- ^ Output information useful for debugging
  } deriving (Eq, Show)

instance Semigroup Config where
  a <> b = Config
    { cfgDynOptions = cfgDynOptions a <> cfgDynOptions b
    , cfgUnsafe = cfgUnsafe a || cfgUnsafe b
    , cfgDebug = cfgDebug a || cfgDebug b
    }

instance Monoid Config where
  mempty = defaultConfig
  mappend = (<>)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    cfgDynOptions <- o .: "ghc-opts"
    cfgUnsafe <- o .: "unsafe"
    cfgDebug <- o .: "debug"
    return Config {..}

-- | Default 'Config'.

defaultConfig :: Config
defaultConfig = Config
  { cfgDynOptions = []
  , cfgUnsafe = False
  , cfgDebug = False
  }

-- | A wrapper for dynamic options.

newtype DynOption = DynOption
  { unDynOption :: String
  } deriving (Eq, Ord, Show, FromJSON)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.

dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

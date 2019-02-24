-- | Configuration options used by the tool.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    cfgDynOptions <- o .: "ghc-opts"
    cfgUnsafe <- o .: "unsafe"
    return Config {..}

-- | Default 'Config'.

defaultConfig :: Config
defaultConfig = Config
  { cfgDynOptions = []
  , cfgUnsafe = False
  }

-- | A wrapper for dynamic options.

newtype DynOption = DynOption
  { unDynOption :: String
  } deriving (Eq, Ord, Show, FromJSON)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.

dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

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
  } deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.

dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

-- | Types used in the library.

module Ormolu.Type
  ( Config (..)
  , defaultConfig
  , DynOption (..)
  , dynOption
  )
where

import qualified SrcLoc as GHC

-- | Ormolu configuration.

newtype Config = Config
  { cfgDynOptions :: [DynOption]
    -- ^ Dynamic options to pass to GHC parser
  } deriving (Eq, Show)

-- | Default 'Config'.

defaultConfig :: Config
defaultConfig = Config
  { cfgDynOptions = []
  }

-- | A wrapper for dynamic options.

newtype DynOption = DynOption
  { unDynOption :: String
  } deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.

dynOption :: DynOption -> GHC.Located String
dynOption (DynOption o) = GHC.L GHC.noSrcSpan o

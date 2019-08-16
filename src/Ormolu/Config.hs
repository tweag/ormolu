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
  , cfgTolerateCpp :: !Bool
    -- ^ Do not fail if CPP pragma is present (still doesn't handle CPP but
    -- useful for formatting of files that enable the extension without
    -- actually containing CPP macros)
  , cfgCheckIdempotency :: !Bool
    -- ^ Checks if re-formatting the result is idempotent.
  } deriving (Eq, Show)

-- | Default 'Config'.

defaultConfig :: Config
defaultConfig = Config
  { cfgDynOptions = []
  , cfgUnsafe = False
  , cfgDebug = False
  , cfgTolerateCpp = False
  , cfgCheckIdempotency = False
  }

-- | A wrapper for dynamic options.

newtype DynOption = DynOption
  { unDynOption :: String
  } deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.

dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

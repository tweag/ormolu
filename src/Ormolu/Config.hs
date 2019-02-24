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
  { cfgDynOptions :: [DynOption]
    -- ^ Dynamic options to pass to GHC parser
  , cfgSanityCheck :: Bool
    -- ^ Whether to parse output of formatter and compare the obtained AST
    -- with original AST. Doing this makes the program much slower, but
    -- it'll catch and report all possible issues.
  } deriving (Eq, Show)

-- | Default 'Config'.

defaultConfig :: Config
defaultConfig = Config
  { cfgDynOptions = []
  , cfgSanityCheck = True
  }

-- | A wrapper for dynamic options.

newtype DynOption = DynOption
  { unDynOption :: String
  } deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.

dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

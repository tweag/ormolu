-- | Configuration options used by the tool.
module Ormolu.Config
  ( Config (..),
    defaultConfig,
    DynOption (..),
    dynOptionToLocatedStr,
  )
where

import qualified SrcLoc as GHC

-- | Ormolu configuration.
data Config = Config
  { -- | Dynamic options to pass to GHC parser
    cfgDynOptions :: ![DynOption],
    -- | Do formatting faster but without automatic detection of defects
    cfgUnsafe :: !Bool,
    -- | Output information useful for debugging
    cfgDebug :: !Bool,
    -- | Checks if re-formatting the result is idempotent.
    cfgCheckIdempotency :: !Bool
  }
  deriving (Eq, Show)

-- | Default 'Config'.
defaultConfig :: Config
defaultConfig =
  Config
    { cfgDynOptions = [],
      cfgUnsafe = False,
      cfgDebug = False,
      cfgCheckIdempotency = False
    }

-- | A wrapper for dynamic options.
newtype DynOption = DynOption
  { unDynOption :: String
  }
  deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.
dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

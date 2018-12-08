module Ormolu.Type
  ( Config (..)
  , DynOption (..)
  , dynOption
  )
where

import Data.Default.Class
import qualified SrcLoc as GHC

-- | Ormolu configuration.

data Config = Config
  { cfgLineWidth :: Int
  , cfgIndentStep :: Int
  , cfgDynOptions :: [DynOption]
  }

instance Default Config where
  def = Config
    { cfgLineWidth = 80
    , cfgIndentStep = 2
    , cfgDynOptions = []
    }

-- | A wrapper for dynamic options.

newtype DynOption = DynOption
  { unDynOption :: String
  } deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.

dynOption :: DynOption -> GHC.Located String
dynOption (DynOption o) = GHC.L GHC.noSrcSpan o

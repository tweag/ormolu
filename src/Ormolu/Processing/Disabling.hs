{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Definitions related to disabling of Ormolu with magic comments.
module Ormolu.Processing.Disabling
  ( OrmoluState (..),
    DisabledRegionLine (..),
    DisabledRegions,
    enableMarker,
    disableMarker,
  )
where

import Data.Data (Data)
import Data.String (IsString (..))

-- | Line in a region where formatting is disabled.
data DisabledRegionLine
  = -- | Line in which indentation can be changed if needed.
    Reindent String
  | -- | Line in which indentation should be preserved.
    NoReindent String
  deriving (Data, Eq, Show)

-- | Cut-out fragments of file where formatting is disabled.
type DisabledRegions = [[DisabledRegionLine]]

-- | Ormolu state.
data OrmoluState
  = -- | Enabled
    OrmoluEnabled
  | -- | Disabled; contains a difference list of lines in the current disabled
    -- region
    OrmoluDisabled ([DisabledRegionLine] -> [DisabledRegionLine])

-- | Canonical enable marker.
enableMarker :: IsString s => s
enableMarker = "{- ORMOLU_ENABLE -}"

-- | Canonical disable marker.
disableMarker :: IsString s => s
disableMarker = "{- ORMOLU_DISABLE -}"

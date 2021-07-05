{-# LANGUAGE OverloadedStrings #-}

-- | Common definitions for pre- and post- processing.
module Ormolu.Processing.Common
  ( DisabledRegions,
    enableMarker,
    disableMarker,
  )
where

import Data.String (IsString (..))

-- | Cut-out fragments of file where formatting is disabled.
type DisabledRegions = [[String]]

-- | Canonical enable marker.
enableMarker :: IsString s => s
enableMarker = "{- ORMOLU_ENABLE -}"

-- | Canonical disable marker.
disableMarker :: IsString s => s
disableMarker = "{- ORMOLU_DISABLE -}"

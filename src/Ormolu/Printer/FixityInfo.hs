{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ormolu.Printer.FixityInfo (FixityInfo (..), defaultFixityInfo) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import GHC.Types.Basic (FixityDirection (..))

-- | Gives fixity information (direction and precedence level) about an infix operator, but takes the uncertainty that can arise from conflicting definitions into account.
data FixityInfo = FixityInfo
  { -- | Fixity direction (InfixL, InfixR, or InfixN (not associative)), if it is known
    fixDir :: Maybe FixityDirection,
    -- | Minimum precedence level found in the (maybe conflicting) definitions for the operator (inclusive)
    fixMinPrec :: Int,
    -- | Maximum precedence level found in the (maybe conflicting) definitions for the operator (inclusive)
    fixMaxPrec :: Int
  }
  deriving (Eq, Generic)

-- | Corresponds to the lowest level of information we can get about an operator (no information for fixity direction, and a precedence between 0 and 9).
defaultFixityInfo :: FixityInfo
defaultFixityInfo =
  FixityInfo
    { fixDir = Nothing,
      fixMinPrec = 0,
      fixMaxPrec = 9
    }

-- | Gives the ability to merge two (maybe conflicting) definitions for an operator, keeping the higher level of compatible information from both.
instance Semigroup FixityInfo where
  FixityInfo {fixDir = dir1, fixMinPrec = min1, fixMaxPrec = max1} <> FixityInfo {fixDir = dir2, fixMinPrec = min2, fixMaxPrec = max2} =
    FixityInfo {fixDir = dir', fixMinPrec = min min1 min2, fixMaxPrec = max max1 max2}
    where
      dir' = case (dir1, dir2) of
        (Just a, Just b) | a == b -> Just a
        _ -> Nothing

deriving instance Generic FixityDirection

instance Hashable FixityDirection

instance Hashable FixityInfo

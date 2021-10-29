module Ormolu.Printer.FixityInfo (FixityInfo (..), defaultFixityInfo) where

import GHC.Types.Basic (FixityDirection)
data FixityInfo = FixityInfo
  { fixDirection :: Maybe FixityDirection,
    fixMinPrec :: Int,
    fixMaxPrec :: Int
  } deriving (Eq)

defaultFixityInfo :: FixityInfo
defaultFixityInfo = FixityInfo
  { fixDirection = Nothing,
    fixMinPrec = 0,
    fixMaxPrec = 9
  }

instance Semigroup FixityInfo where
  FixityInfo{fixDirection=dir1, fixMinPrec=min1, fixMaxPrec=max1} <> FixityInfo{fixDirection=dir2, fixMinPrec=min2, fixMaxPrec=max2} =
    FixityInfo{fixDirection=dir', fixMinPrec=min min1 min2, fixMaxPrec=max max1 max2} where
      dir' = case (dir1, dir2) of
        (Just a, Just b) | a == b -> Just a
        _ -> Nothing
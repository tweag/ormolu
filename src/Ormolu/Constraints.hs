{-# LANGUAGE DeriveDataTypeable #-}

module Ormolu.Constraints
  ( sortConstraints,
  )
where

import Data.Char
import Data.Generics (Data, Typeable, cast, gmapQ)
import Data.List (sortOn)
import Outputable
import GHC

sortConstraints :: [LHsType GhcPs] -> [LHsType GhcPs]
sortConstraints = sortOn oFromL

oFromL :: LHsType GhcPs -> OrdWrapper
oFromL = O . unLoc

newtype OrdWrapper = O (HsType GhcPs)
  deriving (Typeable, Data)

instance Eq OrdWrapper where
  (O a) == (O b) = compare (O a) (O b) == EQ

getIdentifiers :: Data d => d -> [RdrName]
getIdentifiers a = case cast a of
  Just name -> [name]
  Nothing -> mconcat $ case cast a :: Maybe (HsType GhcPs) of
    Just (HsForAllTy _ _ _ body) -> gmapQ getIdentifiers body
    _ -> gmapQ getIdentifiers a

showRdrName :: RdrName -> String
showRdrName = showSDocUnsafe . ppr

getIdentStrings :: Data d => d -> [String]
getIdentStrings = fmap (fmap toLower) . fmap showRdrName . getIdentifiers

instance Ord (OrdWrapper) where
  compare a b = compare (getIdentStrings a) (getIdentStrings b)

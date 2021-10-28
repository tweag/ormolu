module Ormolu.OpTree.OperatorDatabase where

import Data.Map (Map)
import qualified Data.Map as Map

data Fixity
  = InfixL
  | InfixR
  | Infix
  | UnknownFix
  deriving (Eq, Show)

data Precedence
  = PrecUnique Int
  | PrecRange Int Int
  deriving (Eq, Show)

data Range = Range Int Int deriving (Eq, Show)

instance Semigroup Fixity where
  a <> b | a == b = a
  _ <> _ = UnknownFix

instance Semigroup Precedence where
  (PrecUnique a) <> (PrecUnique b)
    | a == b = PrecUnique a
    | otherwise = PrecRange (min a b) (max a b)
  PrecUnique a <> PrecRange min2 max2 = PrecRange (min a min2) (max a max2)
  PrecRange min1 max1 <> PrecUnique a = PrecRange (min min1 a) (max max1 a)
  PrecRange min1 max1 <> PrecRange min2 max2 = PrecRange (min min1 min2) (max max1 max2)

data OperatorInfo = OperatorInfo
  { oprFix :: Fixity,
    oprPrec :: Precedence
  }

instance Semigroup OperatorInfo where
  OperatorInfo {oprFix=oprFix1, oprPrec=oprPrec1} <> OperatorInfo {oprFix=oprFix2, oprPrec=oprPrec2} =
    OperatorInfo {oprFix=oprFix1 <> oprFix2, oprPrec=oprPrec1 <> oprPrec2}
defaultOperatorInfo :: OperatorInfo
defaultOperatorInfo = OperatorInfo {oprFix=InfixL, oprPrec=PrecUnique 9}

operatorDatabase :: Map String OperatorInfo
operatorDatabase =
  Map.fromList [
    (
      "!!!",
      -- infixl 9 in list-tuple, bitwise, natural, records, tidal, neural, ShellCheck, Lastik, inflist, combinat, Agda, statistics-dirichlet, hat, hw-prim, local-search, constructive-algebra, prelude-generalize
      OperatorInfo {oprFix=InfixL, oprPrec=PrecUnique 9}
    )
  ]

module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls,
    hasSeparatedDecls,
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_hsDecls :: FamilyStyle -> [LHsDecl GhcPs] -> R ()

hasSeparatedDecls :: [LHsDecl GhcPs] -> Bool

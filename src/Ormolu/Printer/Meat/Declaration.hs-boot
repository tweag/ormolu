module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls
  , p_hsDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_hsDecls :: FamilyStyle -> [LHsDecl GhcPs] -> R ()

p_hsDecl :: FamilyStyle -> HsDecl GhcPs -> R ()

module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls
  , p_hsDecl
  )
where

import GHC
import Ormolu.Printer.Combinators

p_hsDecls :: [LHsDecl GhcPs] -> R ()

p_hsDecl :: HsDecl GhcPs -> R ()

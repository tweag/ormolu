{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of declarations.

module Ormolu.Printer.Meat.Declaration
  ( p_hsDecl
  )
where

import HsDecls
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Type
import Ormolu.Printer.Meat.Declaration.TypeFamily

p_hsDecl :: HsDecl GhcPs -> R ()
p_hsDecl = \case
  TyClD x -> p_tyClDecl x
  _ -> error "this is not yet supported"

p_tyClDecl :: TyClDecl GhcPs -> R ()
p_tyClDecl = \case
  FamDecl x -> p_famDecl x
  SynDecl {..} -> p_synDecl tcdLName tcdTyVars tcdRhs
  _ -> error "this is not yet supported"

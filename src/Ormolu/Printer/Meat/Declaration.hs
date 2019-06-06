{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of declarations.

module Ormolu.Printer.Meat.Declaration
  ( p_hsDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Data
import Ormolu.Printer.Meat.Declaration.Instance
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Type
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Utils

p_hsDecl :: HsDecl GhcPs -> R ()
p_hsDecl = \case
  TyClD NoExt x -> p_tyClDecl x
  ValD NoExt x -> p_valDecl x
  SigD NoExt x -> p_sigDecl x
  InstD NoExt x -> p_instDecl x
  _ -> notImplemented "certain kinds of declarations"

p_tyClDecl :: TyClDecl GhcPs -> R ()
p_tyClDecl = \case
  FamDecl NoExt x -> p_famDecl x
  SynDecl {..} -> p_synDecl tcdLName tcdTyVars tcdRhs
  DataDecl {..} -> p_dataDecl tcdLName tcdTyVars tcdDataDefn
  _ -> notImplemented "certain kinds of declarations"

p_instDecl :: InstDecl GhcPs -> R ()
p_instDecl = \case
  ClsInstD NoExt x -> p_clsInstDecl x
  TyFamInstD NoExt x -> p_tyFamInstDecl Free x
  _ -> notImplemented "certain kinds of declarations"

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of declarations.

module Ormolu.Printer.Meat.Declaration
  ( p_hsDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Annotation
import Ormolu.Printer.Meat.Declaration.Class
import Ormolu.Printer.Meat.Declaration.Data
import Ormolu.Printer.Meat.Declaration.Default
import Ormolu.Printer.Meat.Declaration.Instance
import Ormolu.Printer.Meat.Declaration.RoleAnnotation
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Type
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_hsDecl :: HsDecl GhcPs -> R ()
p_hsDecl = \case
  TyClD NoExt x -> p_tyClDecl x
  ValD NoExt x -> p_valDecl x
  SigD NoExt x -> p_sigDecl x
  InstD NoExt x -> p_instDecl x
  DerivD NoExt x -> p_derivDecl x
  DefD NoExt x -> p_defaultDecl x
  ForD _ _ -> notImplemented "ForD"
  WarningD _ _ -> notImplemented "WarningD"
  AnnD NoExt x -> p_annDecl x
  RuleD _ _ -> notImplemented "RuleD"
  SpliceD _ _ -> notImplemented "SpliceD"
  DocD _ _ -> notImplemented "DocD"
  RoleAnnotD NoExt x -> p_roleAnnot x
  XHsDecl _ -> notImplemented "XHsDecl"

p_tyClDecl :: TyClDecl GhcPs -> R ()
p_tyClDecl = \case
  FamDecl NoExt x -> p_famDecl Free x
  SynDecl {..} -> p_synDecl tcdLName tcdTyVars tcdRhs
  DataDecl {..} ->
    p_dataDecl Associated tcdLName (tyVarsToTypes tcdTyVars) tcdDataDefn
  ClassDecl {..} ->
    p_classDecl
      tcdCtxt
      tcdLName
      tcdTyVars
      tcdFDs
      tcdSigs
      tcdMeths
      tcdATs
      tcdATDefs
  XTyClDecl {} -> notImplemented "XTyClDecl"

p_instDecl :: InstDecl GhcPs -> R ()
p_instDecl = \case
  ClsInstD NoExt x -> p_clsInstDecl x
  TyFamInstD NoExt x -> p_tyFamInstDecl Free x
  DataFamInstD NoExt x -> p_dataFamInstDecl Free x
  XInstDecl _ -> notImplemented "XInstDecl"

p_derivDecl :: DerivDecl GhcPs -> R ()
p_derivDecl = \case
  d@DerivDecl {..} -> p_standaloneDerivDecl d
  XDerivDecl _ -> notImplemented "XDerivDecl standalone deriving"

----------------------------------------------------------------------------
-- Helpers

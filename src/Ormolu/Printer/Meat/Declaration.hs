{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of declarations.

module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls
  , p_hsDecl
  )
where

import Control.Monad (forM_)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Annotation
import Ormolu.Printer.Meat.Declaration.Class
import Ormolu.Printer.Meat.Declaration.Data
import Ormolu.Printer.Meat.Declaration.Default
import Ormolu.Printer.Meat.Declaration.Foreign
import Ormolu.Printer.Meat.Declaration.Instance
import Ormolu.Printer.Meat.Declaration.RoleAnnotation
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Splice
import Ormolu.Printer.Meat.Declaration.Type
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_hsDecls :: [LHsDecl GhcPs] -> R ()
p_hsDecls decls =
  forM_ (zip decls ((Just <$> drop 1 decls) ++ [Nothing])) $
    \(d, md) -> do
      case md of
        Nothing -> located d p_hsDecl
        Just d' ->
          if separatedDecls (unLoc d) (unLoc d')
            then line (located d p_hsDecl)
            else located d p_hsDecl

p_hsDecl :: HsDecl GhcPs -> R ()
p_hsDecl = \case
  TyClD NoExt x -> p_tyClDecl x
  ValD NoExt x -> p_valDecl x
  SigD NoExt x -> p_sigDecl x
  InstD NoExt x -> p_instDecl x
  DerivD NoExt x -> p_derivDecl x
  DefD NoExt x -> p_defaultDecl x
  ForD NoExt x -> p_foreignDecl x
  WarningD _ _ -> notImplemented "WarningD"
  AnnD NoExt x -> p_annDecl x
  RuleD _ _ -> notImplemented "RuleD"
  SpliceD NoExt x -> p_spliceDecl x
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
      tcdFixity
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

-- | Determine if these declarations should be separated by a blank line.

separatedDecls
  :: HsDecl GhcPs
  -> HsDecl GhcPs
  -> Bool
separatedDecls (TypeSignature n) (FunctionBody n') = n /= n'
separatedDecls (FunctionBody n) x | Just n' <- isPragma x = n /= n'
separatedDecls x y | Just n <- isPragma x, Just n' <- isPragma y = n /= n'
separatedDecls x (TypeSignature n') | Just n <- isPragma x = n /= n'
separatedDecls _ _ = True

isPragma
  :: HsDecl GhcPs
  -> (Maybe RdrName)
isPragma = \case
  InlinePragma n -> Just n
  SpecializePragma n -> Just n
  SCCPragma n -> Just n
  _ -> Nothing

pattern TypeSignature
      , FunctionBody
      , InlinePragma
      , SpecializePragma
      , SCCPragma :: RdrName -> HsDecl GhcPs
pattern TypeSignature n <- SigD NoExt (TypeSig NoExt ((L _ n):_) _)
pattern FunctionBody n <- ValD NoExt (FunBind NoExt (L _ n) _ _ _)
pattern InlinePragma n <- SigD NoExt (InlineSig NoExt (L _ n) _)
pattern SpecializePragma n <- SigD NoExt (SpecSig NoExt (L _ n) _ _)
pattern SCCPragma n <- SigD NoExt (SCCFunSig NoExt _ (L _ n) _)

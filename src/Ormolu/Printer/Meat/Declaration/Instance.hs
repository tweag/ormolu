{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type class instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl
  , FamilyStyle (Associate, Free)
  , p_tyFamInstDecl
  )
where

import BasicTypes
import Control.Arrow
import Data.Foldable
import Data.Function
import Data.List (sortBy)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_clsInstDecl :: ClsInstDecl GhcPs -> R ()
p_clsInstDecl = \case
  ClsInstDecl {..} -> do
    txt "instance "
    case unLoc <$> cid_overlap_mode of
      Just Overlappable {} -> txt "{-# OVERLAPPABLE #-} "
      Just Overlapping {} -> txt "{-# OVERLAPPING #-} "
      Just Overlaps {} -> txt "{-# OVERLAPS #-} "
      Just Incoherent {} -> txt "{-# INCOHERENT #-} "
      _ -> pure ()
    case cid_poly_ty of
      HsIB {..} -> sitcc (located hsib_body p_hsType)
      XHsImplicitBndrs NoExt -> notImplemented "XHsImplicitBndrs"
    -- GHC's AST does not necessarily store each kind of element in source
    -- location order. This happens because different declarations are stored in
    -- different lists. Consequently, to get all the declarations in proper
    -- order, they need to be manually sorted.
    let binds = (getLoc &&& located' p_valDecl) <$> cid_binds
        sigs = (getLoc &&& located' p_sigDecl) <$> cid_sigs
        tyfam_insts =
          (getLoc &&& located' (p_tyFamInstDecl Associate)) <$> cid_tyfam_insts
        decls =
          snd <$>
          sortBy (compare `on` fst) (toList binds <> sigs <> tyfam_insts)
    if not (null decls)
      then do
        txt " where"
        newline
        inci (sequence_ decls)
      else do
        newline
  XClsInstDecl NoExt -> notImplemented "XClsInstDecl"

data FamilyStyle
  = Associate
  | Free

p_tyFamInstDecl :: FamilyStyle -> TyFamInstDecl GhcPs -> R ()
p_tyFamInstDecl style = \case
  TyFamInstDecl {..} -> do
    txt $ case style of
      Associate -> "type "
      Free -> "type instance "
    p_tyFamInstEqn tfid_eqn
    newline

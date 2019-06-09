{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type class, type family, and data family instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl
  , p_tyFamInstDecl
  , p_dataFamInstDecl
  , p_standaloneDerivDecl
  )
where

import BasicTypes
import Control.Arrow
import Data.Foldable
import Data.Function
import Data.List (sortBy)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Data
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_standaloneDerivDecl :: DerivDecl GhcPs -> R ()
p_standaloneDerivDecl DerivDecl {..} = do
  let typesAfterInstance = located (hsib_body (hswc_body deriv_type)) p_hsType
      instTypes toIndent = inci $ do
        txt "instance"
        breakpoint
        match_overlap_mode deriv_overlap_mode breakpoint
        if toIndent
          then inci typesAfterInstance
          else typesAfterInstance
  txt "deriving"
  case deriv_strategy of
    Nothing -> do
      space
      instTypes False
    Just l -> locatedVia Nothing l $ \case
      StockStrategy -> do
        txt " stock "
        instTypes False
      AnyclassStrategy -> do
        txt " anyclass "
        instTypes False
      NewtypeStrategy -> do
        txt " newtype "
        instTypes False
      ViaStrategy HsIB {..} -> do
        txt " via"
        breakpoint
        inci (located hsib_body p_hsType)
        breakpoint
        instTypes True
      ViaStrategy (XHsImplicitBndrs NoExt) ->
        notImplemented "XHsImplicitBndrs"
  newline
p_standaloneDerivDecl (XDerivDecl _) = notImplemented "XDerivDecl"

p_clsInstDecl :: ClsInstDecl GhcPs -> R ()
p_clsInstDecl = \case
  ClsInstDecl {..} -> do
    txt "instance "
    match_overlap_mode cid_overlap_mode space
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
          (getLoc &&& located' (p_tyFamInstDecl Associated)) <$>
          cid_tyfam_insts
        datafam_insts =
          (getLoc &&& located' (p_dataFamInstDecl Associated)) <$>
          cid_datafam_insts
        decls =
          snd <$>
          sortBy
            (compare `on` fst)
            (toList binds <> sigs <> tyfam_insts <> datafam_insts)
    if not (null decls)
      then do
        txt " where"
        newline
        inci (sequence_ decls)
      else do
        newline
  XClsInstDecl NoExt -> notImplemented "XClsInstDecl"

p_tyFamInstDecl :: FamilyStyle -> TyFamInstDecl GhcPs -> R ()
p_tyFamInstDecl style = \case
  TyFamInstDecl {..} -> do
    txt $ case style of
      Associated -> "type"
      Free -> "type instance"
    breakpoint
    inci (p_tyFamInstEqn tfid_eqn)
    newline

p_dataFamInstDecl :: FamilyStyle -> DataFamInstDecl GhcPs -> R ()
p_dataFamInstDecl style = \case
  DataFamInstDecl {..} -> do
    let HsIB {..} = dfid_eqn
        FamEqn {..} = hsib_body
    p_dataDecl style feqn_tycon feqn_pats feqn_rhs

match_overlap_mode :: Maybe (Located OverlapMode) -> R () -> R ()
match_overlap_mode overlap_mode layoutStrategy =
  case unLoc <$> overlap_mode of
    Just Overlappable {} -> do
      txt "{-# OVERLAPPABLE #-}"
      layoutStrategy
    Just Overlapping {} -> do
      txt "{-# OVERLAPPING #-}"
      layoutStrategy
    Just Overlaps {} -> do
      txt "{-# OVERLAPS #-}"
      layoutStrategy
    Just Incoherent {} -> do
      txt "{-# INCOHERENT #-}"
      layoutStrategy
    _ -> pure ()

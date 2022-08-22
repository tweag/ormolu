{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Type class, type family, and data family instance declarations.
module Ormolu.Printer.Meat.Declaration.Instance
  ( p_clsInstDecl,
    p_tyFamInstDecl,
    p_dataFamInstDecl,
    p_standaloneDerivDecl,
  )
where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Function (on)
import Data.List (sortBy)
import GHC.Hs
import GHC.Types.Basic
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Data
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Type

p_standaloneDerivDecl :: DerivDecl GhcPs -> R ()
p_standaloneDerivDecl DerivDecl {..} = do
  let typesAfterInstance = located (hswc_body deriv_type) p_hsSigType
      instTypes toIndent = inci $ do
        txt "instance"
        breakpoint
        match_overlap_mode deriv_overlap_mode breakpoint
        inciIf toIndent typesAfterInstance
  txt "deriving"
  space
  case deriv_strategy of
    Nothing ->
      instTypes False
    Just (L _ a) -> case a of
      StockStrategy _ -> do
        txt "stock "
        instTypes False
      AnyclassStrategy _ -> do
        txt "anyclass "
        instTypes False
      NewtypeStrategy _ -> do
        txt "newtype "
        instTypes False
      ViaStrategy (XViaStrategyPs _ sigTy) -> do
        txt "via"
        breakpoint
        inci (located sigTy p_hsSigType)
        breakpoint
        instTypes True

p_clsInstDecl :: ClsInstDecl GhcPs -> R ()
p_clsInstDecl ClsInstDecl {..} = do
  txt "instance"
  -- GHC's AST does not necessarily store each kind of element in source
  -- location order. This happens because different declarations are stored in
  -- different lists. Consequently, to get all the declarations in proper
  -- order, they need to be manually sorted.
  let sigs = (getLocA &&& fmap (SigD NoExtField)) <$> cid_sigs
      vals = (getLocA &&& fmap (ValD NoExtField)) <$> toList cid_binds
      tyFamInsts =
        ( getLocA &&& fmap (InstD NoExtField . TyFamInstD NoExtField)
        )
          <$> cid_tyfam_insts
      dataFamInsts =
        ( getLocA &&& fmap (InstD NoExtField . DataFamInstD NoExtField)
        )
          <$> cid_datafam_insts
      allDecls =
        snd <$> sortBy (leftmost_smallest `on` fst) (sigs <> vals <> tyFamInsts <> dataFamInsts)
  located cid_poly_ty $ \sigTy -> do
    breakpoint
    inci $ do
      match_overlap_mode cid_overlap_mode breakpoint
      p_hsSigType sigTy
      unless (null allDecls) $ do
        breakpoint
        txt "where"
  unless (null allDecls) . inci $ do
    -- Ensure whitespace is added after where clause.
    breakpoint
    dontUseBraces $ p_hsDeclsRespectGrouping Associated allDecls

p_tyFamInstDecl :: FamilyStyle -> TyFamInstDecl GhcPs -> R ()
p_tyFamInstDecl style TyFamInstDecl {..} = do
  txt $ case style of
    Associated -> "type"
    Free -> "type instance"
  breakpoint
  inci (p_tyFamInstEqn tfid_eqn)

p_dataFamInstDecl :: FamilyStyle -> DataFamInstDecl GhcPs -> R ()
p_dataFamInstDecl style (DataFamInstDecl {dfid_eqn = FamEqn {..}}) =
  p_dataDecl style feqn_tycon feqn_pats feqn_fixity feqn_rhs

match_overlap_mode :: Maybe (LocatedP OverlapMode) -> R () -> R ()
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

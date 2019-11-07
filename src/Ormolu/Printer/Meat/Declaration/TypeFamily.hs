{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of data\/type families.
module Ormolu.Printer.Meat.Declaration.TypeFamily
  ( p_famDecl,
    p_tyFamInstEqn,
  )
where

import BasicTypes (LexicalFixity (..))
import Control.Monad
import Data.Maybe (isJust, isNothing)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils
import SrcLoc (GenLocated (..), Located)

p_famDecl :: FamilyStyle -> FamilyDecl GhcPs -> R ()
p_famDecl style FamilyDecl {fdTyVars = HsQTvs {..}, ..} = do
  mmeqs <- case fdInfo of
    DataFamily -> Nothing <$ txt "data"
    OpenTypeFamily -> Nothing <$ txt "type"
    ClosedTypeFamily eqs -> Just eqs <$ txt "type"
  txt $ case style of
    Associated -> mempty
    Free -> " family"
  breakpoint
  inci $ do
    switchLayout (getLoc fdLName : (getLoc <$> hsq_explicit)) $
      p_infixDefHelper
        (isInfix fdFixity)
        inci
        (p_rdrName fdLName)
        (located' p_hsTyVarBndr <$> hsq_explicit)
    let rsig = p_familyResultSigL fdResultSig
    unless (isNothing rsig && isNothing fdInjectivityAnn) $
      space
    inci $ do
      sequence_ rsig
      when (isJust rsig && isJust fdInjectivityAnn) breakpoint
      forM_ fdInjectivityAnn (located' p_injectivityAnn)
  case mmeqs of
    Nothing -> return ()
    Just meqs -> do
      space
      txt "where"
      case meqs of
        Nothing -> do
          space
          txt ".."
        Just eqs -> do
          newline
          sep newline (located' (inci . p_tyFamInstEqn)) eqs
p_famDecl _ FamilyDecl {fdTyVars = XLHsQTyVars {}} =
  notImplemented "XLHsQTyVars"
p_famDecl _ (XFamilyDecl NoExt) = notImplemented "XFamilyDecl"

p_familyResultSigL ::
  Located (FamilyResultSig GhcPs) ->
  Maybe (R ())
p_familyResultSigL l =
  case l of
    L _ a -> case a of
      NoSig NoExt -> Nothing
      KindSig NoExt k -> Just $ do
        txt "::"
        breakpoint
        located k p_hsType
      TyVarSig NoExt bndr -> Just $ do
        txt "="
        breakpoint
        located bndr p_hsTyVarBndr
      XFamilyResultSig NoExt ->
        notImplemented "XFamilyResultSig"

p_injectivityAnn :: InjectivityAnn GhcPs -> R ()
p_injectivityAnn (InjectivityAnn a bs) = do
  txt "|"
  space
  p_rdrName a
  space
  txt "->"
  space
  sep space p_rdrName bs

p_tyFamInstEqn :: TyFamInstEqn GhcPs -> R ()
p_tyFamInstEqn HsIB {hsib_body = FamEqn {..}} = do
  case feqn_bndrs of
    Nothing -> return ()
    Just bndrs -> do
      p_forallBndrs p_hsTyVarBndr bndrs
      breakpoint
  (if null feqn_bndrs then id else inci) $ do
    let famLhsSpn = getLoc feqn_tycon : fmap (getLoc . typeArgToType) feqn_pats
    switchLayout famLhsSpn $
      p_infixDefHelper
        (isInfix feqn_fixity)
        inci
        (p_rdrName feqn_tycon)
        (located' p_hsType . typeArgToType <$> feqn_pats)
    space
    txt "="
    breakpoint
    inci (located feqn_rhs p_hsType)
p_tyFamInstEqn HsIB {hsib_body = XFamEqn {}} = notImplemented "HsIB XFamEqn"
p_tyFamInstEqn (XHsImplicitBndrs NoExt) = notImplemented "XHsImplicitBndrs"

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

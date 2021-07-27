{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of data\/type families.
module Ormolu.Printer.Meat.Declaration.TypeFamily
  ( p_famDecl,
    p_tyFamInstEqn,
  )
where

import Control.Monad
import Data.Maybe (isNothing)
import GHC.Hs.Decls
import GHC.Hs.Extension
import GHC.Hs.Type
import GHC.Types.Basic
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type

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
        True
        (p_rdrName fdLName)
        (located' p_hsTyVarBndr <$> hsq_explicit)
    let resultSig = p_familyResultSigL fdResultSig
    unless (isNothing resultSig && isNothing fdInjectivityAnn) space
    inci $ do
      sequence_ resultSig
      space
      forM_ fdInjectivityAnn (located' p_injectivityAnn)
  case mmeqs of
    Nothing -> return ()
    Just meqs -> do
      inci $ do
        breakpoint
        txt "where"
      case meqs of
        Nothing -> do
          space
          txt ".."
        Just eqs -> do
          newline
          sep newline (located' (inci . p_tyFamInstEqn)) eqs

p_familyResultSigL ::
  Located (FamilyResultSig GhcPs) ->
  Maybe (R ())
p_familyResultSigL (L _ a) = case a of
  NoSig NoExtField -> Nothing
  KindSig NoExtField k -> Just $ do
    txt "::"
    breakpoint
    located k p_hsType
  TyVarSig NoExtField bndr -> Just $ do
    equals
    breakpoint
    located bndr p_hsTyVarBndr

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
      p_forallBndrs ForAllInvis p_hsTyVarBndr bndrs
      breakpoint
  inciIf (not $ null feqn_bndrs) $ do
    let famLhsSpn = getLoc feqn_tycon : fmap lhsTypeArgSrcSpan feqn_pats
    switchLayout famLhsSpn $
      p_infixDefHelper
        (isInfix feqn_fixity)
        True
        (p_rdrName feqn_tycon)
        (p_lhsTypeArg <$> feqn_pats)
    space
    equals
    breakpoint
    inci (located feqn_rhs p_hsType)

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

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
import GHC.Hs
import GHC.Types.Fixity
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
  let headerSpns = getLocA fdLName : (getLocA <$> hsq_explicit)
      headerAndSigSpns = getLocA fdResultSig : headerSpns
  inci . switchLayout headerAndSigSpns $ do
    breakpoint
    switchLayout headerSpns $ do
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
      inci . switchLayout headerAndSigSpns $ do
        breakpoint
        txt "where"
      case meqs of
        Nothing -> do
          space
          txt ".."
        Just eqs -> do
          newline
          inci (sep newline (located' p_tyFamInstEqn) eqs)

p_familyResultSigL ::
  LFamilyResultSig GhcPs ->
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
p_injectivityAnn (InjectivityAnn _ a bs) = do
  txt "|"
  space
  p_rdrName a
  space
  txt "->"
  space
  sep space p_rdrName bs

p_tyFamInstEqn :: TyFamInstEqn GhcPs -> R ()
p_tyFamInstEqn FamEqn {..} = do
  case feqn_bndrs of
    HsOuterImplicit NoExtField -> return ()
    HsOuterExplicit _ bndrs -> do
      p_forallBndrs ForAllInvis p_hsTyVarBndr bndrs
      breakpoint
  let atLeastOneBndr = case feqn_bndrs of
        HsOuterImplicit NoExtField -> False
        HsOuterExplicit _ bndrs -> not $ null bndrs
  inciIf atLeastOneBndr $ do
    let famLhsSpn = getLocA feqn_tycon : fmap lhsTypeArgSrcSpan feqn_pats
    switchLayout famLhsSpn $
      p_infixDefHelper
        (isInfix feqn_fixity)
        True
        (p_rdrName feqn_tycon)
        (p_lhsTypeArg <$> feqn_pats)
    inci $ do
      space
      equals
      breakpoint
      located feqn_rhs p_hsType

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

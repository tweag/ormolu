{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of data\/type families.

module Ormolu.Printer.Meat.Declaration.TypeFamily
  ( p_famDecl
  , p_tyFamInstEqn
  )
where

import BasicTypes (LexicalFixity (..))
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing, isJust)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils
import SrcLoc (Located, GenLocated (..))

p_famDecl :: FamilyStyle -> FamilyDecl GhcPs -> R ()
p_famDecl style FamilyDecl {..} = do
  mmeqs <- case fdInfo of
    DataFamily -> Nothing <$ txt "data"
    OpenTypeFamily -> Nothing <$ txt "type"
    ClosedTypeFamily eqs -> Just eqs <$ txt "type"
  txt $ case style of
    Associated -> mempty
    Free -> " family"
  let HsQTvs {..} = fdTyVars
      combinedSpans = combineSrcSpans' $
        getSpan fdLName :| fmap getSpan hsq_explicit
  breakpoint
  inci $ do
    switchLayout combinedSpans $ do
      p_infixDefHelper
        (isInfix fdFixity)
        inci
        (p_rdrName fdLName)
        (located' p_hsTyVarBndr <$> hsq_explicit)
    let rsig = p_familyResultSigL (isJust fdInjectivityAnn) fdResultSig
    unless (isNothing rsig && isNothing fdInjectivityAnn) $
      breakpoint
    inci $ do
      sequence_ rsig
      when (isJust rsig && isJust fdInjectivityAnn) breakpoint
      forM_ fdInjectivityAnn (located' p_injectivityAnn)
  case mmeqs of
    Nothing -> newline
    Just meqs -> do
      txt " where"
      case meqs of
        Nothing -> txt " .." >> newline
        Just eqs -> do
          newline
          forM_ eqs (located' (line . inci . p_tyFamInstEqn))
p_famDecl _ (XFamilyDecl NoExt) = notImplemented "XFamilyDecl"

p_familyResultSigL
  :: Bool
  -> Located (FamilyResultSig GhcPs)
  -> Maybe (R ())
p_familyResultSigL injAnn l =
  case l of
    L _ a -> case a of
      NoSig NoExt -> Nothing
      KindSig NoExt k -> Just $ do
        if injAnn then txt "= " else txt ":: "
        located k p_hsType
      TyVarSig NoExt bndr -> Just $ do
        if injAnn then txt "= " else txt ":: "
        located bndr p_hsTyVarBndr
      XFamilyResultSig NoExt ->
        notImplemented "XFamilyResultSig"

p_injectivityAnn :: InjectivityAnn GhcPs -> R ()
p_injectivityAnn (InjectivityAnn a bs) = do
  txt "| "
  p_rdrName a
  space
  txt "-> "
  spaceSep p_rdrName bs

p_tyFamInstEqn :: TyFamInstEqn GhcPs -> R ()
p_tyFamInstEqn HsIB {..} = do
  let FamEqn {..} = hsib_body
      combinedSpans = combineSrcSpans' $
        getSpan feqn_tycon :| fmap getSpan feqn_pats
  switchLayout combinedSpans $ p_infixDefHelper
    (isInfix feqn_fixity)
    inci
    (p_rdrName feqn_tycon)
    (located' p_hsType <$> feqn_pats)
  txt " ="
  breakpoint
  inci (located feqn_rhs p_hsType)
p_tyFamInstEqn (XHsImplicitBndrs NoExt) = notImplemented "XHsImplicitBndrs"

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of data\/type families.

module Ormolu.Printer.Meat.Declaration.TypeFamily
  ( p_famDecl
  )
where

import Control.Monad
import Data.Maybe (maybeToList, isJust)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils
import SrcLoc (Located, GenLocated (..))

p_famDecl :: FamilyDecl GhcPs -> R ()
p_famDecl FamilyDecl {..} = do
  mmeqs <- case fdInfo of
    DataFamily -> Nothing <$ txt "data family "
    OpenTypeFamily -> Nothing <$ txt "type family "
    ClosedTypeFamily eqs -> Just eqs <$ txt "type family "
  p_rdrName fdLName
  space
  let HsQTvs {..} = fdTyVars
      items =
        maybeToList (p_familyResultSigL (isJust fdInjectivityAnn) fdResultSig)
          ++ (located' p_injectivityAnn <$> maybeToList fdInjectivityAnn)
  spaceSep (located' p_hsTyVarBndr) hsq_explicit
  unless (null items) $
    breakpoint
  inci . inci $ spaceSep id items
  case mmeqs of
    Nothing -> newline
    Just meqs -> do
      txt " where"
      case meqs of
        Nothing -> txt " .." >> newline
        Just eqs -> do
          newline
          forM_ eqs (located' (line . inci . p_tyFamInstEqn))
p_famDecl (XFamilyDecl NoExt) = notImplemented "XFamilyDecl"

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
  p_rdrName feqn_tycon
  space
  spaceSep (located' p_hsType) feqn_pats
  txt " ="
  breakpoint
  inci (located feqn_rhs p_hsType)
p_tyFamInstEqn (XHsImplicitBndrs NoExt) = notImplemented "XHsImplicitBndrs"

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Renedring of data type declarations.

module Ormolu.Printer.Meat.Declaration.Data
  ( p_dataDecl
  )
where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils
import RdrName (RdrName (..))
import SrcLoc (Located)

p_dataDecl
  :: Located RdrName            -- ^ Type constructor
  -> LHsQTyVars GhcPs           -- ^ Type variables
  -> HsDataDefn GhcPs           -- ^ Data definition
  -> R ()
p_dataDecl name tvars HsDataDefn {..} = do
  let HsQTvs {..} = tvars
  txt $ case dd_ND of
    NewType -> "newtype "
    DataType -> "data "
  p_rdrName name
  unless (null hsq_explicit) space
  spaceSep (located' p_hsTyVarBndr) hsq_explicit
  case dd_kindSig of
    Nothing -> return ()
    Just k -> do
      space
      txt ":: "
      located k p_hsType
  let gadt = isJust dd_kindSig || any (isGadt . unL) dd_cons
  if gadt
    then do
      txt " where"
      newline
      inci $ newlineSep (sitcc . located' p_conDecl) dd_cons
    else switchLayout (combineSrcSpans' (getSpan name :| (getSpan <$> dd_cons))) $ do
      breakpoint
      inci $ do
        txt "= "
        let sep = vlayout (txt " | ") (txt "| ")
        velt $ withSep sep (sitcc . located' p_conDecl) dd_cons
  newline
  inci . located dd_derivs $ \xs ->
    forM_ xs (line . located' p_hsDerivingClause)
p_dataDecl _ _ (XHsDataDefn NoExt) = notImplemented "XHsDataDefn"

p_conDecl :: ConDecl GhcPs -> R ()
p_conDecl = \case
  ConDeclGADT {..} -> do
    spaceSep p_rdrName con_names
    breakpoint
    inci $ do
      txt ":: "
      p_forallBndrs (hsq_explicit con_qvars)
      forM_ con_mb_cxt p_lhsContext
      case con_args of
        PrefixCon xs -> do
          velt' (located' p_hsType <$> xs)
          unless (null xs) $ do
            breakpoint
            txt "-> "
        RecCon l -> do
          located l p_conDeclFields
          unless (null $ unL l) $ do
            breakpoint
            txt "-> "
        InfixCon _ _ -> notImplemented "InfixCon"
      locatedVia Nothing con_res_ty p_hsType
  ConDeclH98 {..} -> do
    p_forallBndrs con_ex_tvs
    forM_ con_mb_cxt p_lhsContext
    case con_args of
      PrefixCon xs -> do
        p_rdrName con_name
        unless (null xs) breakpoint
        inci $ velt' (located' p_hsType <$> xs)
      RecCon l -> do
        p_rdrName con_name
        breakpoint
        inci $ located l p_conDeclFields
      InfixCon x y -> do
        located x p_hsType
        breakpoint
        inci $ do
          p_rdrName con_name
          space
          located y p_hsType
  XConDecl NoExt -> notImplemented "XConDecl"

p_forallBndrs
  :: [LHsTyVarBndr GhcPs]
  -> R ()
p_forallBndrs = \case
  [] -> return ()
  bndrs -> do
    txt "forall "
    spaceSep (located' p_hsTyVarBndr) bndrs
    txt ". "

p_lhsContext
  :: LHsContext GhcPs
  -> R ()
p_lhsContext = \case
  L _ [] -> pure ()
  ctx -> do
    located ctx p_hsContext
    breakpoint
    txt "=> "

isGadt :: ConDecl GhcPs -> Bool
isGadt = \case
  ConDeclGADT {} -> True
  ConDeclH98 {} -> False
  XConDecl {} -> False

p_hsDerivingClause
  :: HsDerivingClause GhcPs
  -> R ()
p_hsDerivingClause HsDerivingClause {..} = do
  txt "deriving"
  let derivingWhat = located deriv_clause_tys $ \case
        [] -> txt "()"
        [x] -> located (hsib_body x) p_hsType
        xs -> parens . velt $ withSep comma (located' p_hsType . hsib_body) xs
  case deriv_clause_strategy of
    Nothing -> do
      breakpoint
      inci derivingWhat
    Just l -> locatedVia Nothing l $ \case
      StockStrategy -> do
        txt " stock"
        breakpoint
        inci derivingWhat
      AnyclassStrategy -> do
        txt " anyclass"
        breakpoint
        inci derivingWhat
      NewtypeStrategy -> do
        txt " newtype"
        breakpoint
        inci derivingWhat
      ViaStrategy HsIB {..} -> do
        breakpoint
        inci $ do
          derivingWhat
          breakpoint
          txt "via "
          located hsib_body p_hsType
      ViaStrategy (XHsImplicitBndrs NoExt) ->
        notImplemented "XHsImplicitBndrs"
p_hsDerivingClause (XHsDerivingClause NoExt) = notImplemented "XHsDerivingClause"

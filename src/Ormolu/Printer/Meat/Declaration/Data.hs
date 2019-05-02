{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Renedring of data type declarations.

module Ormolu.Printer.Meat.Declaration.Data
  ( p_dataDecl
  )
where

import BasicTypes (DerivStrategy (..))
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils (unL, getSpan, combineSrcSpans')
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
  located name p_rdrName'
  unless (null hsq_explicit) space
  spaceSep (located' p_hsTyVarBndr) hsq_explicit
  case dd_kindSig of
    Nothing -> return ()
    Just k -> do
      space
      txt ":: "
      relaxComments (located k p_hsType)
  let gadt = isJust dd_kindSig || any (isGadt . unL) dd_cons
  if gadt
    then do
      txt " where"
      newline
      inci $ newlineSep (located' p_conDecl) dd_cons
    else switchLayout (combineSrcSpans' (getSpan name :| (getSpan <$> dd_cons))) $ do
      breakpoint
      inci $ do
        txt "= "
        let sep = vlayout (txt " | ") (txt "| ")
        velt $ withSep sep (located' p_conDecl) dd_cons
  newline
  inci . located dd_derivs $ \xs ->
    forM_ xs (line . located' p_hsDerivingClause)

p_conDecl :: ConDecl GhcPs -> R ()
p_conDecl = \case
  ConDeclGADT {..} -> velt'
    [ spaceSep (located' p_rdrName) con_names
    , inci $ do
        txt ":: "
        relaxComments (locatedVia Nothing (hsib_body con_type) p_hsType)
    ]
  ConDeclH98 {..} -> do
    case hsq_explicit <$> con_qvars of
      Nothing -> return ()
      Just bndrs -> do
        txt "forall "
        spaceSep (located' p_hsTyVarBndr) bndrs
        txt "."
        breakpoint
    case con_cxt of
      Nothing -> return ()
      Just ctx -> located ctx $ \case
        [] -> pure ()
        xs -> do
          p_hsContext xs
          breakpoint
          txt "=> "
    case con_details of
      PrefixCon xs -> do
        located con_name p_rdrName
        unless (null xs) breakpoint
        inci $ velt' (located' p_hsType <$> xs)
      RecCon l -> do
        located con_name p_rdrName
        breakpoint
        inci $ located l p_conDeclFields
      InfixCon x y -> velt'
        [ located x p_hsType
        , inci $ velt'
          [ backticks (located con_name p_rdrName)
          , inci $ located y p_hsType
          ]
        ]

isGadt :: ConDecl GhcPs -> Bool
isGadt = \case
  ConDeclGADT {} -> True
  ConDeclH98 {} -> False

p_hsDerivingClause
  :: HsDerivingClause GhcPs
  -> R ()
p_hsDerivingClause HsDerivingClause {..} = do
  txt "deriving"
  case deriv_clause_strategy of
    Nothing -> return ()
    Just l -> do
      space
      located l $ \case
        StockStrategy -> txt "stock"
        AnyclassStrategy -> txt "anyclass"
        NewtypeStrategy -> txt "newtype"
  breakpoint
  inci . located deriv_clause_tys $ \case
    [] -> txt "()"
    xs -> parens . velt $ withSep comma (located' p_hsType . hsib_body) xs

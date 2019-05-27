{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ormolu.Printer.Meat.Declaration.Pat
  ( p_pat
  )
where

import BasicTypes
import Control.Monad
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils

p_pat :: Pat GhcPs -> R ()
p_pat = \case
  WildPat NoExt -> txt "_"
  VarPat NoExt name -> located name p_rdrName
  LazyPat NoExt pat -> do
    txt "~"
    located pat p_pat
  AsPat NoExt name pat -> do
    located name p_rdrName
    txt "@"
    located pat p_pat
  ParPat NoExt pat ->
    located pat (parens . p_pat)
  BangPat NoExt pat -> do
    txt "!"
    located pat p_pat
  ListPat NoExt pats -> do
    brackets $ velt (withSep comma (located' p_pat) pats)
  TuplePat NoExt pats boxing -> do
    let f =
          case boxing of
            Boxed -> parens
            Unboxed -> parensHash
    f $ velt (withSep comma (located' p_pat) pats)
  SumPat NoExt pat _ _ -> do
    -- XXX I'm not sure about this one.
    located pat p_pat
  ConPatIn pat details ->
    case details of
      PrefixCon xs -> do
        located pat p_rdrName
        unless (null xs) . inci . inci $ do
          breakpoint
          velt' (located' p_pat <$> xs)
      RecCon (HsRecFields fields dotdot) -> do
        located pat p_rdrName
        case dotdot of
          Nothing -> txt " {..}"
          Just _ -> do
            braces $ velt (withSep comma (located' p_hsRecField) fields)
      InfixCon x y -> do
        located x p_pat
        located pat (backticks . p_rdrName)
        located y p_pat
  ConPatOut {} -> notImplemented "ConPatOut"
  ViewPat {} -> notImplemented "ViewPat"
  SplicePat {} -> notImplemented "SplicePat"
  LitPat NoExt p -> atom p
  NPat NoExt v _ _ -> located v (atom . ol_val)
  NPlusKPat {} -> notImplemented "NPlusKPat"
  SigPat {} -> notImplemented "SigPat"
  CoPat {} -> notImplemented "CoPat"
  XPat NoExt -> notImplemented "XPat"

p_hsRecField :: HsRecField' (FieldOcc GhcPs) (LPat GhcPs) -> R ()
p_hsRecField HsRecField {..} =
  located hsRecFieldLbl $ \x ->
    located (rdrNameFieldOcc x) p_rdrName

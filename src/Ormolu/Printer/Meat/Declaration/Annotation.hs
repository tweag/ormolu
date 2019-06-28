{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Annotation
  ( p_annDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Utils

p_annDecl :: AnnDecl GhcPs -> R ()
p_annDecl = \case
  HsAnnotation NoExt _ annProv expr -> line . pragma "ANN" $ do
    p_annProv annProv
    breakpoint
    located expr p_hsExpr
  XAnnDecl {} -> notImplemented "XAnnDecl"

p_annProv :: AnnProvenance (IdP GhcPs) -> R ()
p_annProv = \case
  ValueAnnProvenance name -> p_rdrName name
  TypeAnnProvenance name -> p_rdrName name
  ModuleAnnProvenance -> txt "module"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Annotation
  ( p_annDecl,
  )
where

import GHC.Hs.Decls
import GHC.Hs.Extension
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Value

p_annDecl :: AnnDecl GhcPs -> R ()
p_annDecl (HsAnnotation NoExtField _ annProv expr) =
  pragma "ANN" . inci $ do
    p_annProv annProv
    breakpoint
    located expr p_hsExpr

p_annProv :: AnnProvenance (IdP GhcPs) -> R ()
p_annProv = \case
  ValueAnnProvenance name -> p_rdrName name
  TypeAnnProvenance name -> txt "type" >> space >> p_rdrName name
  ModuleAnnProvenance -> txt "module"

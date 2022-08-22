{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Rendering of Role annotation declarations.
module Ormolu.Printer.Meat.Declaration.RoleAnnotation
  ( p_roleAnnot,
  )
where

import GHC.Core.Coercion.Axiom
import GHC.Hs hiding (anns)
import GHC.Types.Name.Reader
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_roleAnnot :: RoleAnnotDecl GhcPs -> R ()
p_roleAnnot (RoleAnnotDecl _ l_name anns) = p_roleAnnot' l_name anns

p_roleAnnot' :: LocatedN RdrName -> [XRec GhcPs (Maybe Role)] -> R ()
p_roleAnnot' l_name anns = do
  txt "type role"
  breakpoint
  inci $ do
    p_rdrName l_name
    breakpoint
    let p_role' = maybe (txt "_") p_role
    inci . sitcc $ sep breakpoint (sitcc . located' p_role') anns

p_role :: Role -> R ()
p_role = \case
  Nominal -> txt "nominal"
  Representational -> txt "representational"
  Phantom -> txt "phantom"

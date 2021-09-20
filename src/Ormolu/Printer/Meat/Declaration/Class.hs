{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of type class declarations.
module Ormolu.Printer.Meat.Declaration.Class
  ( p_classDecl,
  )
where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import GHC.Hs
import GHC.Types.Fixity
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Type

p_classDecl ::
  Maybe (LHsContext GhcPs) ->
  LocatedN RdrName ->
  LHsQTyVars GhcPs ->
  LexicalFixity ->
  [LHsFunDep GhcPs] ->
  [LSig GhcPs] ->
  LHsBinds GhcPs ->
  [LFamilyDecl GhcPs] ->
  [LTyFamDefltDecl GhcPs] ->
  [LDocDecl GhcPs] ->
  R ()
p_classDecl ctx name HsQTvs {..} fixity fdeps csigs cdefs cats catdefs cdocs = do
  let variableSpans = getLocA <$> hsq_explicit
      signatureSpans = getLocA name : variableSpans
      dependencySpans = getLocA <$> fdeps
      combinedSpans = maybeToList (getLocA <$> ctx) ++ signatureSpans ++ dependencySpans
      -- GHC's AST does not necessarily store each kind of element in source
      -- location order. This happens because different declarations are stored
      -- in different lists. Consequently, to get all the declarations in proper
      -- order, they need to be manually sorted.
      sigs = (getLocA &&& fmap (SigD NoExtField)) <$> csigs
      vals = (getLocA &&& fmap (ValD NoExtField)) <$> toList cdefs
      tyFams = (getLocA &&& fmap (TyClD NoExtField . FamDecl NoExtField)) <$> cats
      docs = (getLocA &&& fmap (DocD NoExtField)) <$> cdocs
      tyFamDefs =
        ( getLocA &&& fmap (InstD NoExtField . TyFamInstD NoExtField)
        )
          <$> catdefs
      allDecls =
        snd <$> sortBy (leftmost_smallest `on` fst) (sigs <> vals <> tyFams <> tyFamDefs <> docs)
  txt "class"
  switchLayout combinedSpans $ do
    breakpoint
    inci $ do
      for_ ctx p_classContext
      switchLayout signatureSpans $
        p_infixDefHelper
          (isInfix fixity)
          True
          (p_rdrName name)
          (located' p_hsTyVarBndr <$> hsq_explicit)
      inci (p_classFundeps fdeps)
      unless (null allDecls) $ do
        breakpoint
        txt "where"
  unless (null allDecls) $ do
    breakpoint -- Ensure whitespace is added after where clause.
    inci (p_hsDeclsRespectGrouping Associated allDecls)

p_classContext :: LHsContext GhcPs -> R ()
p_classContext ctx = unless (null (unLoc ctx)) $ do
  located ctx p_hsContext
  space
  txt "=>"
  breakpoint

p_classFundeps :: [LHsFunDep GhcPs] -> R ()
p_classFundeps fdeps = unless (null fdeps) $ do
  breakpoint
  txt "|"
  space
  inci $ sep commaDel (sitcc . located' p_funDep) fdeps

p_funDep :: FunDep GhcPs -> R ()
p_funDep (FunDep _ before after) = do
  sep space p_rdrName before
  space
  txt "->"
  space
  sep space p_rdrName after

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

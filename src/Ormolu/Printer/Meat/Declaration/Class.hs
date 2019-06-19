{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of type class declarations.

module Ormolu.Printer.Meat.Declaration.Class
  ( p_classDecl
  )
where

import Class
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List (sortBy)
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils
import GHC
import RdrName (RdrName (..))
import SrcLoc (Located, combineSrcSpans)

p_classDecl
  :: LHsContext GhcPs
  -> Located RdrName
  -> LHsQTyVars GhcPs
  -> LexicalFixity
  -> [Located (FunDep (Located RdrName))]
  -> [LSig GhcPs]
  -> LHsBinds GhcPs
  -> [LFamilyDecl GhcPs]
  -> [LTyFamDefltEqn GhcPs]
  -> R ()
p_classDecl ctx name tvars fixity fdeps csigs cdefs cats catdefs = do
  let HsQTvs {..} = tvars
      variableSpans = foldr (combineSrcSpans . getLoc) noSrcSpan hsq_explicit
      signatureSpans = getLoc name `combineSrcSpans` variableSpans
      dependencySpans = foldr (combineSrcSpans . getLoc) noSrcSpan fdeps
      combinedSpans =
        getLoc ctx `combineSrcSpans`
        signatureSpans `combineSrcSpans`
        dependencySpans
  txt "class "
  sitcc $ do
    switchLayout combinedSpans $ p_classContext ctx
    switchLayout signatureSpans $ do
      p_infixDefHelper
        (isInfix fixity)
        inci
        (p_rdrName name)
        (located' p_hsTyVarBndr <$> hsq_explicit)
    switchLayout combinedSpans $ p_classFundeps fdeps
  -- GHC's AST does not necessarily store each kind of element in source
  -- location order. This happens because different declarations are stored in
  -- different lists. Consequently, to get all the declarations in proper order,
  -- they need to be manually sorted.
  let sigs = (getLoc &&& located' p_sigDecl) <$> csigs
      defs = (getLoc &&& located' p_valDecl) <$> cdefs
      tyfam_decls = (getLoc &&& located' (p_famDecl Associated)) <$> cats
      tyfam_defs = (getLoc &&& located' p_famDefDecl) <$> catdefs
      decls =
        snd <$>
        sortBy
          (compare `on` fst)
          (sigs <> toList defs <> tyfam_decls <> tyfam_defs)
  if not (null decls)
    then do
      txt " where"
      newline
      inci (sequence_ decls)
    else newline

p_classContext :: LHsContext GhcPs -> R ()
p_classContext ctx = unless (null (unLoc ctx)) $ do
  located ctx p_hsContext
  breakpoint
  txt "=> "

p_classFundeps :: [Located (FunDep (Located RdrName))] -> R ()
p_classFundeps fdeps = unless (null fdeps) $ do
  breakpoint
  txt "| "
  velt $ withSep comma (located' p_funDep) fdeps

p_famDefDecl :: TyFamDefltEqn GhcPs -> R ()
p_famDefDecl FamEqn {..} = do
  txt "type"
  breakpoint
  let eqn = FamEqn { feqn_pats = tyVarsToTypes feqn_pats, .. }
      hsib = HsIB { hsib_ext = NoExt, hsib_body = eqn }
  inci (p_tyFamInstEqn hsib)
  newline
p_famDefDecl XFamEqn {} = notImplemented "XFamEqn"

p_funDep :: FunDep (Located RdrName) -> R ()
p_funDep (before, after) = do
  spaceSep p_rdrName before
  txt " -> "
  spaceSep p_rdrName after

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

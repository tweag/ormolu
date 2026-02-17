{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Printer.Meat.Declaration.Rule
  ( p_ruleDecls,
    p_ruleBndrs,
  )
where

import GHC.Hs
import GHC.Types.Basic
import GHC.Types.SourceText
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type

p_ruleDecls :: RuleDecls GhcPs -> R ()
p_ruleDecls (HsRules _ xs) =
  pragma "RULES" $ sep breakpoint (sitcc . located' p_ruleDecl) xs

p_ruleDecl :: RuleDecl GhcPs -> R ()
p_ruleDecl (HsRule _ ruleName activation ruleBndrs lhs rhs) = do
  located ruleName p_ruleName
  space
  p_activation activation
  space
  p_ruleBndrs ruleBndrs
  breakpoint
  inci $ do
    located lhs p_hsExpr
    space
    equals
    inci $ do
      breakpoint
      located rhs p_hsExpr

p_ruleName :: RuleName -> R ()
p_ruleName name = atom (HsString NoSourceText name :: HsLit GhcPs)

p_ruleBndrs :: RuleBndrs GhcPs -> R ()
p_ruleBndrs (RuleBndrs HsRuleBndrsAnn {..} tyvars ruleBndrs) = do
  case tyvars of
    Nothing -> return ()
    Just xs -> do
      p_forallBndrs ForAllInvis p_hsTyVarBndr xs
      space
  case rb_tmanns of
    Nothing -> pure ()
    Just _ -> p_forallBndrs ForAllInvis p_ruleBndr ruleBndrs

p_ruleBndr :: RuleBndr GhcPs -> R ()
p_ruleBndr = \case
  RuleBndr _ x -> p_rdrName x
  RuleBndrSig _ x HsPS {..} -> parens N $ do
    p_rdrName x
    p_typeAscription (lhsTypeToSigType hsps_body)

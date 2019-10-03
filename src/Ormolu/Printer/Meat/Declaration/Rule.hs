{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Printer.Meat.Declaration.Rule
  ( p_ruleDecls,
  )
where

import BasicTypes
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Utils

p_ruleDecls :: RuleDecls GhcPs -> R ()
p_ruleDecls = \case
  HsRules NoExt _ xs ->
    pragma "RULES" . sitcc $
      sep breakpoint (sitcc . located' p_ruleDecl) xs
  XRuleDecls NoExt -> notImplemented "XRuleDecls"

p_ruleDecl :: RuleDecl GhcPs -> R ()
p_ruleDecl = \case
  HsRule NoExt ruleName activation ruleBndrs lhs rhs -> do
    located ruleName p_ruleName
    space
    p_activation activation
    space
    p_ruleBndrs ruleBndrs
    breakpoint
    inci $ do
      located lhs p_hsExpr
      space
      txt "="
      inci $ do
        breakpoint
        located rhs p_hsExpr
  XRuleDecl NoExt -> notImplemented "XRuleDecl"

p_ruleName :: (SourceText, RuleName) -> R ()
p_ruleName (_, name) = atom $ HsString NoSourceText name

p_ruleBndrs :: [LRuleBndr GhcPs] -> R ()
p_ruleBndrs [] = return ()
p_ruleBndrs bndrs =
  switchLayout (getLoc <$> bndrs) $ do
    txt "forall"
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p_ruleBndr) bndrs
      txt "."

p_ruleBndr :: RuleBndr GhcPs -> R ()
p_ruleBndr = \case
  RuleBndr NoExt x -> p_rdrName x
  RuleBndrSig NoExt x hswc -> parens N $ do
    p_rdrName x
    p_typeAscription hswc
  XRuleBndr NoExt -> notImplemented "XRuleBndr"

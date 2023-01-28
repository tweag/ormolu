{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Printer.Meat.Declaration.Rule
  ( p_ruleDecls,
  )
where

import Control.Monad (unless)
import GHC.Hs
import GHC.Types.Basic
import GHC.Types.SourceText
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Type

p_ruleDecls :: RuleDecls GhcPs -> R ()
p_ruleDecls (HsRules _ xs) =
  pragma "RULES" $ sep breakpoint (sitcc . located' p_ruleDecl) xs

p_ruleDecl :: RuleDecl GhcPs -> R ()
p_ruleDecl (HsRule _ ruleName activation tyvars ruleBndrs lhs rhs) = do
  located ruleName p_ruleName
  space
  p_activation activation
  space
  case tyvars of
    Nothing -> return ()
    Just xs -> do
      p_forallBndrs ForAllInvis p_hsTyVarBndr xs
      space
  -- It appears that there is no way to tell if there was an empty forall
  -- in the input or no forall at all. We do not want to add redundant
  -- foralls, so let's just skip the empty ones.
  unless (null ruleBndrs) $
    p_forallBndrs ForAllInvis p_ruleBndr ruleBndrs
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

p_ruleBndr :: RuleBndr GhcPs -> R ()
p_ruleBndr = \case
  RuleBndr _ x -> p_rdrName x
  RuleBndrSig _ x HsPS {..} -> parens N $ do
    p_rdrName x
    p_typeAscription (lhsTypeToSigType hsps_body)

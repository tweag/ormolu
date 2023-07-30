module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl,
    p_pat,
    p_hsExpr,
    p_hsUntypedSplice,
    p_stringLit,
    p_hsExpr',
    p_hsCmdTop,
    exprPlacement,
    cmdTopPlacement,
  )
where

import GHC.Data.FastString
import GHC.Hs
import Ormolu.Printer.Combinators

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_pat :: Pat GhcPs -> R ()
p_hsExpr :: HsExpr GhcPs -> R ()
p_hsUntypedSplice :: SpliceDecoration -> HsUntypedSplice GhcPs -> R ()
p_stringLit :: FastString -> R ()

data IsApplicand

p_hsExpr' :: IsApplicand -> BracketStyle -> HsExpr GhcPs -> R ()
p_hsCmdTop :: BracketStyle -> HsCmdTop GhcPs -> R ()
exprPlacement :: HsExpr GhcPs -> Placement
cmdTopPlacement :: HsCmdTop GhcPs -> Placement

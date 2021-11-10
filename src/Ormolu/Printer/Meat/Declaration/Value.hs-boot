module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl,
    p_pat,
    p_hsExpr,
    p_hsSplice,
    p_stringLit,
    p_hsExpr',
    p_hsCmdTop,
    exprPlacement,
    cmdTopPlacement,
  )
where

import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Extension
import GHC.Hs.Pat
import Ormolu.Printer.Combinators

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_pat :: Pat GhcPs -> R ()
p_hsExpr :: HsExpr GhcPs -> R ()
p_hsSplice :: HsSplice GhcPs -> R ()
p_stringLit :: String -> R ()
p_hsExpr' :: BracketStyle -> HsExpr GhcPs -> R ()
p_hsCmdTop :: BracketStyle -> HsCmdTop GhcPs -> R ()
exprPlacement :: HsExpr GhcPs -> Placement
cmdTopPlacement :: HsCmdTop GhcPs -> Placement

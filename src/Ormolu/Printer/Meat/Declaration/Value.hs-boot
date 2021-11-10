module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl,
    p_pat,
    p_hsExpr,
    p_hsSplice,
    p_stringLit,
    p_tyOpTree,
    tyOpTree,
  )
where

import GHC.Hs (LHsType)
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Extension
import GHC.Hs.Pat
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (Located)
import Ormolu.Printer.Combinators
import Ormolu.Printer.Operators (OpSubTreeInfo, OpTree)

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_pat :: Pat GhcPs -> R ()
p_hsExpr :: HsExpr GhcPs -> R ()
p_hsSplice :: HsSplice GhcPs -> R ()
p_stringLit :: String -> R ()
p_tyOpTree :: OpTree (LHsType GhcPs) (OpSubTreeInfo (LHsType GhcPs) (Located RdrName)) -> R ()
tyOpTree :: LHsType GhcPs -> OpTree (LHsType GhcPs) (Located RdrName)

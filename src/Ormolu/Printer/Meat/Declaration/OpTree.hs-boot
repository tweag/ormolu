module Ormolu.Printer.Meat.Declaration.OpTree
  ( p_exprOpTree,
    exprOpTree,
    p_cmdOpTree,
    cmdOpTree,
    p_tyOpTree,
    tyOpTree,
    getOpName,
    getOpNameStr,
  )
where

import GHC.Hs (GhcPs, HsExpr, LHsCmdTop, LHsExpr, LHsType, LocatedN)
import GHC.Types.Name.Reader (RdrName)
import Ormolu.Printer.Combinators (BracketStyle, R)
import Ormolu.Printer.Operators (OpInfo, OpTree)

exprOpTree :: LHsExpr GhcPs -> OpTree (LHsExpr GhcPs) (LHsExpr GhcPs)
p_exprOpTree ::
  BracketStyle ->
  OpTree (LHsExpr GhcPs) (OpInfo (LHsExpr GhcPs)) ->
  R ()
cmdOpTree :: LHsCmdTop GhcPs -> OpTree (LHsCmdTop GhcPs) (LHsExpr GhcPs)
p_cmdOpTree ::
  BracketStyle ->
  OpTree (LHsCmdTop GhcPs) (OpInfo (LHsExpr GhcPs)) ->
  R ()
tyOpTree :: LHsType GhcPs -> OpTree (LHsType GhcPs) (LocatedN RdrName)
p_tyOpTree ::
  OpTree (LHsType GhcPs) (OpInfo (LocatedN RdrName)) ->
  R ()
getOpName :: HsExpr GhcPs -> Maybe RdrName
getOpNameStr :: RdrName -> String

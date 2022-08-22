{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Printing of operator trees.
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

import Data.Functor ((<&>))
import GHC.Hs
import GHC.Types.Fixity
import GHC.Types.Name (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common (p_rdrName)
import Ormolu.Printer.Meat.Declaration.Value
  ( cmdTopPlacement,
    exprPlacement,
    p_hsCmdTop,
    p_hsExpr,
    p_hsExpr',
  )
import Ormolu.Printer.Meat.Type (p_hsType)
import Ormolu.Printer.Operators
import Ormolu.Utils (HasSrcSpan)

-- | Extract the operator name of the specified 'HsExpr' if this expression
-- corresponds to an operator.
getOpName :: HsExpr GhcPs -> Maybe RdrName
getOpName = \case
  HsVar _ (L _ a) -> Just a
  _ -> Nothing

-- | Convert an operator name to a 'String'.
getOpNameStr :: RdrName -> String
getOpNameStr = occNameString . rdrNameOcc

-- | Decide if the operands of an operator chain should be hanging.
opBranchPlacement ::
  HasSrcSpan l =>
  -- | Placer function for nodes
  (ty -> Placement) ->
  -- | first expression of the chain
  OpTree (GenLocated l ty) op ->
  -- | last expression of the chain
  OpTree (GenLocated l ty) op ->
  Placement
opBranchPlacement placer firstExpr lastExpr
  -- If the beginning of the first argument and the last argument starts on
  -- the same line, and the second argument has a hanging form, use hanging
  -- placement.
  | isOneLineSpan
      ( mkSrcSpan
          (srcSpanStart (opTreeLoc firstExpr))
          (srcSpanStart (opTreeLoc lastExpr))
      ),
    OpNode (L _ n) <- lastExpr =
      placer n
  | otherwise = Normal

-- | Decide whether to use braces or not based on the layout and placement
-- of an expression in an infix operator application.
opBranchBraceStyle :: Placement -> R (R () -> R ())
opBranchBraceStyle placement =
  getLayout <&> \case
    SingleLine -> useBraces
    MultiLine -> case placement of
      Hanging -> useBraces
      Normal -> dontUseBraces

-- | Convert a 'LHsExpr' containing an operator tree to the 'OpTree'
-- intermediate representation.
exprOpTree :: LHsExpr GhcPs -> OpTree (LHsExpr GhcPs) (LHsExpr GhcPs)
exprOpTree (L _ (OpApp _ x op y)) = OpBranches [exprOpTree x, exprOpTree y] [op]
exprOpTree n = OpNode n

-- | Print an operator tree where leaves are values.
p_exprOpTree ::
  -- | Bracket style to use
  BracketStyle ->
  -- | N-ary 'OpTree' to render, enhanced with information regarding
  -- operator fixity
  OpTree (LHsExpr GhcPs) (OpInfo (LHsExpr GhcPs)) ->
  R ()
p_exprOpTree s (OpNode x) = located x (p_hsExpr' s)
p_exprOpTree s t@(OpBranches exprs ops) = do
  let firstExpr = head exprs
      otherExprs = tail exprs
      placement =
        opBranchPlacement
          exprPlacement
          firstExpr
          (last otherExprs)
      rightMostNode = \case
        n@(OpNode _) -> n
        OpBranches exprs'' _ -> rightMostNode (last exprs'')
      isDoBlock = \case
        OpNode (L _ (HsDo _ ctx _)) -> case ctx of
          DoExpr _ -> True
          MDoExpr _ -> True
          _ -> False
        _ -> False
      -- Whether we could place the operator in a trailing position,
      -- followed by a breakpoint before the RHS
      couldBeTrailing (prevExpr, opi) =
        -- An operator with fixity InfixR 0, like seq, $, and $ variants,
        -- is required
        isHardSplitterOp (opiFix opi)
          -- the LHS must be single-line
          && isOneLineSpan (opTreeLoc prevExpr)
          -- can only happen when a breakpoint would have been added anyway
          && placement == Normal
          -- if the node just on the left of the operator (so the rightmost
          -- node of the subtree prevExpr) is a do-block, then we cannot
          -- place the operator in a trailing position (because it would be
          -- read as being part of the do-block)
          && not (isDoBlock $ rightMostNode prevExpr)
      -- If all operators at the current level match the conditions to be
      -- trailing, then put them in a trailing position
      isTrailing = all couldBeTrailing $ zip exprs ops
  ub <- if isTrailing then return useBraces else opBranchBraceStyle placement
  let p_x = ub $ p_exprOpTree s firstExpr
      putOpsExprs prevExpr (opi : ops') (expr : exprs') = do
        let isLast = null exprs'
            ub' = if not isLast then ub else id
            -- Distinguish holes used in infix notation.
            -- eg. '1 _foo 2' and '1 `_foo` 2'
            opWrapper = case unLoc (opiOp opi) of
              HsUnboundVar _ _ -> backticks
              _ -> id
            p_op = located (opiOp opi) (opWrapper . p_hsExpr)
            p_y = ub' $ p_exprOpTree N expr
        if isTrailing
          then do
            space
            p_op
            placeHanging
              -- When we have a chain of trailing operators (staircase style),
              -- the last operand, when multiline, is allowed to hang
              -- (ex: do block, lambda...)
              ( if isLast && (not . isOneLineSpan . opTreeLoc $ expr)
                  then opBranchPlacement exprPlacement prevExpr expr
                  else Normal
              )
              $ do
                p_y
                putOpsExprs expr ops' exprs'
          else do
            placeHanging placement $ do
              p_op
              space
              p_y
            putOpsExprs expr ops' exprs'
      putOpsExprs _ _ _ = pure ()
  switchLayout [opTreeLoc t] $ do
    p_x
    putOpsExprs firstExpr ops otherExprs

-- | Convert a 'LHsCmdTop' containing an operator tree to the 'OpTree'
-- intermediate representation.
cmdOpTree :: LHsCmdTop GhcPs -> OpTree (LHsCmdTop GhcPs) (LHsExpr GhcPs)
cmdOpTree = \case
  (L _ (HsCmdTop _ (L _ (HsCmdArrForm _ op Infix _ [x, y])))) ->
    OpBranches [cmdOpTree x, cmdOpTree y] [op]
  n -> OpNode n

-- | Print an operator tree where leaves are commands.
p_cmdOpTree ::
  -- | Bracket style to use
  BracketStyle ->
  -- | N-ary OpTree to render, enhanced with information regarding operator
  -- fixity
  OpTree (LHsCmdTop GhcPs) (OpInfo (LHsExpr GhcPs)) ->
  R ()
p_cmdOpTree s (OpNode x) = located x (p_hsCmdTop s)
p_cmdOpTree s t@(OpBranches exprs ops) = do
  let firstExpr = head exprs
      otherExprs = tail exprs
      placement =
        opBranchPlacement
          cmdTopPlacement
          firstExpr
          (last otherExprs)
  ub <- opBranchBraceStyle placement
  let p_x = ub $ p_cmdOpTree s firstExpr
      putOpsExprs (opi : ops') (expr : exprs') = do
        let ub' = if not (null exprs') then ub else id
            p_op = located (opiOp opi) p_hsExpr
            p_y = ub' $ p_cmdOpTree N expr
        placeHanging placement $ do
          p_op
          space
          p_y
        putOpsExprs ops' exprs'
      putOpsExprs _ _ = pure ()
  switchLayout [opTreeLoc t] $ do
    p_x
    putOpsExprs ops otherExprs

-- | Check if given expression has a hanging form. Added for symmetry with
-- exprPlacement and cmdTopPlacement, which are all used in p_xxxOpTree
-- functions with opBranchPlacement.
tyOpPlacement :: HsType GhcPs -> Placement
tyOpPlacement = \case
  _ -> Normal

-- | Convert a LHsType containing an operator tree to the 'OpTree'
-- intermediate representation.
tyOpTree :: LHsType GhcPs -> OpTree (LHsType GhcPs) (LocatedN RdrName)
tyOpTree (L _ (HsOpTy _ _ l op r)) =
  OpBranches [tyOpTree l, tyOpTree r] [op]
tyOpTree n = OpNode n

-- | Print an operator tree where leaves are types.
p_tyOpTree ::
  -- | N-ary 'OpTree' to render, enhanced with information regarding
  -- operator fixity
  OpTree (LHsType GhcPs) (OpInfo (LocatedN RdrName)) ->
  R ()
p_tyOpTree (OpNode n) = located n p_hsType
p_tyOpTree t@(OpBranches exprs ops) = do
  let firstExpr = head exprs
      otherExprs = tail exprs
      placement =
        opBranchPlacement
          tyOpPlacement
          firstExpr
          (last otherExprs)
      p_x = p_tyOpTree firstExpr
      putOpsExprs (opi : ops') (expr : exprs') = do
        let p_op = p_rdrName (opiOp opi)
            p_y = p_tyOpTree expr
        placeHanging
          placement
          $ do
            p_op
            space
            p_y
        putOpsExprs ops' exprs'
      putOpsExprs _ _ = pure ()
  switchLayout [opTreeLoc t] $ do
    ub <- opBranchBraceStyle placement
    ub p_x
    putOpsExprs ops otherExprs

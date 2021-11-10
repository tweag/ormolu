{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.Maybe (fromMaybe)
import GHC.Hs
import GHC.Types.Fixity
import GHC.Types.Name (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common (p_rdrName)
import Ormolu.Printer.Meat.Declaration.Value (cmdTopPlacement, exprPlacement, p_hsCmdTop, p_hsExpr, p_hsExpr')
import Ormolu.Printer.Meat.Type (p_hsType)
import Ormolu.Printer.Operators
import Ormolu.Utils (HasSrcSpan, getLoc')

getOpName :: HsExpr GhcPs -> Maybe RdrName
getOpName = \case
  HsVar _ (L _ a) -> Just a
  _ -> Nothing

getOpNameStr :: RdrName -> String
getOpNameStr = occNameString . rdrNameOcc

opBranchPlacement ::
  HasSrcSpan l =>
  -- | Whether all nodes of the n-ary subtree this OpBranch was part of are starting on the same line in the original file
  Bool ->
  -- | Last node of the n-ary subtree this branch this OpBranch was part of, if it is not itself an OpTree
  Maybe (GenLocated l ty) ->
  -- | Placer function for nodes
  (ty -> Placement) ->
  -- | Left branch
  OpTree (GenLocated l ty) op ->
  -- | Right branch
  OpTree (GenLocated l ty) op ->
  Placement
opBranchPlacement allStNodesStartingSameLine lastStNode placer x y
  -- handle this case:
  -- x = 1 + 2 + 3 + do
  --   4
  -- where every node of the operator chain starts on the same line, and where the last node should be placed in a hanging position even though it is multiline (so the global source span of the tree is multiline)
  | allStNodesStartingSameLine
      && ((fromMaybe False $ (== Hanging) . placer . unLoc <$> lastStNode)) =
      Hanging
  -- If the beginning of the first argument and the second argument are on
  -- the same line, and the second argument has a hanging form, use hanging
  -- placement.
  | isOneLineSpan (mkSrcSpan (srcSpanStart (opTreeLoc x)) (srcSpanStart (opTreeLoc y))),
    OpNode (L _ n) <- y =
      placer n
  | otherwise = Normal

opBranchBraceStyle :: Placement -> R (R () -> R ())
opBranchBraceStyle placement =
  getLayout <&> \case
    SingleLine -> useBraces
    MultiLine -> case placement of
      Hanging -> useBraces
      Normal -> dontUseBraces

exprOpTree :: LHsExpr GhcPs -> OpTree (LHsExpr GhcPs) (LHsExpr GhcPs)
exprOpTree (L _ (OpApp _ x op y)) = OpBranch (exprOpTree x) op (exprOpTree y)
exprOpTree n = OpNode n

p_exprOpTree ::
  -- | Whether the LHS of the current subtree will be printed just after dollar-like operator placed in a trailing position
  Bool ->
  -- | Bracket style to use
  BracketStyle ->
  -- | Binary OpTree to render, enhanced with information regarding operator fixity and n-ary tree context
  OpTree (LHsExpr GhcPs) (OpSubTreeInfo (LHsExpr GhcPs) (LHsExpr GhcPs)) ->
  R ()
p_exprOpTree _ s (OpNode x) = located x (p_hsExpr' s)
p_exprOpTree isParentDollarLikeTrailing s (OpBranch x OpSubTreeInfo {..} y) = do
  let placement = opBranchPlacement stAllNodesStartingSameLine stLastNode exprPlacement x y
      -- Distinguish holes used in infix notation.
      -- eg. '1 _foo 2' and '1 `_foo` 2'
      opWrapper = case unLoc opOp of
        HsUnboundVar _ _ -> backticks
        _ -> id
      p_op = located opOp (opWrapper . p_hsExpr)
      -- If the LHS of the current subtree is just after a dollar-like operator placed in a trailing position, then the LHS of its left child will also be in that situation
      p_x = p_exprOpTree isParentDollarLikeTrailing s x
      -- But the LHS of its right child will only be in this situation if the operator of the subtree at hand is a dollar-like operator placed in a trailing position
      p_y = p_exprOpTree dollarLikeTrailing N y
      isDoBlock = \case
        OpNode (L _ (HsDo _ ctx _)) -> case ctx of
          DoExpr _ -> True
          MDoExpr _ -> True
          _ -> False
        _ -> False
      xSubTreeIsSingleLine = case x of
        OpNode n -> isOneLineSpan (getLoc' n)
        OpBranch _ OpSubTreeInfo {stSpan = xStSpan} _ -> isOneLineSpan xStSpan
      rightMostNode = \case
        n@(OpNode _) -> n
        OpBranch _ _ r -> rightMostNode r
      -- Whether we should place the operator in a trailing position, followed by a breakpoint before the RHS
      dollarLikeTrailing =
        -- An operator with fixity InfixR 0, like seq, $, and $ variants, is required
        isHardSplitterOp opFix
          -- the LHS must be either a single-line leaf, or something part of a single-line n-ary optree
          && xSubTreeIsSingleLine
          -- can only happen when a breakpoint would have been added anyway
          && placement == Normal
          -- if the node just on the left of the operator (so the rightmost node of the subtree x) is a do-block, then we cannot place the operator in a trailing position (because it would be read as being part of the do-block)
          && not (isDoBlock $ rightMostNode x)
  ub <- opBranchBraceStyle placement
  -- The OpTree inherits the layout of the n-ary subtree it was part of. This ensures that a globally multiline "+" chain won't allow multiple additions on a single line
  switchLayout [stSpan] $ do
    if
        | dollarLikeTrailing -> do
            useBraces p_x
            space
            p_op
            breakpoint
            inci p_y
        | otherwise -> do
            ub p_x
            -- An indentation bump for the op + RHS is only required when:
            --   + we are on the left edge of the global OpTree, because an indent has not been produced yet by a parent context
            --   + the operator just before this subtree is dollar-like and placed in a trailing position, and so this subtree needs to be formatted as it was a whole tree (i.e with an indentation bump for subsequent lines)
            placeHanging' (opOnBinaryTreeLeftEdge || isParentDollarLikeTrailing) placement $ do
              p_op
              space
              p_y

pattern CmdTopCmd :: HsCmd GhcPs -> LHsCmdTop GhcPs
pattern CmdTopCmd cmd <- (L _ (HsCmdTop _ (L _ cmd)))

cmdOpTree :: LHsCmdTop GhcPs -> OpTree (LHsCmdTop GhcPs) (LHsExpr GhcPs)
cmdOpTree = \case
  CmdTopCmd (HsCmdArrForm _ op Infix _ [x, y]) ->
    OpBranch (cmdOpTree x) op (cmdOpTree y)
  n -> OpNode n

p_cmdOpTree ::
  -- | Bracket style to use
  BracketStyle ->
  -- | Binary OpTree to render, enhanced with information regarding operator fixity and n-ary tree context
  OpTree (LHsCmdTop GhcPs) (OpSubTreeInfo (LHsCmdTop GhcPs) (LHsExpr GhcPs)) ->
  R ()
p_cmdOpTree s (OpNode x) = located x (p_hsCmdTop s)
p_cmdOpTree s (OpBranch x OpSubTreeInfo {..} y) = do
  let placement = opBranchPlacement stAllNodesStartingSameLine stLastNode cmdTopPlacement x y
      p_op = located opOp p_hsExpr
      p_x = p_cmdOpTree s x
      p_y = p_cmdOpTree N y
  ub <- opBranchBraceStyle placement
  -- The OpTree inherits the layout of the n-ary subtree it was part of. This ensures that a globally multiline "+" chain won't allow multiple additions on a single line
  switchLayout [stSpan] $ do
    ub p_x
    -- An indentation bump for the op + RHS is only required when we are on the left edge of the global OpTree, because an indent has not been produced yet by a parent context
    placeHanging' opOnBinaryTreeLeftEdge placement $ do
      p_op
      space
      p_y

-- | Check if given expression has a hanging form. Added for symmetry with exprPlacement and cmdTopPlacement, which are all used in p_xxxOpTree functions with opBranchPlacement.
tyOpPlacement :: HsType GhcPs -> Placement
tyOpPlacement = \case
  _ -> Normal

tyOpTree :: LHsType GhcPs -> OpTree (LHsType GhcPs) (LocatedN RdrName)
tyOpTree (L _ (HsOpTy NoExtField l op r)) =
  OpBranch (tyOpTree l) op (tyOpTree r)
tyOpTree n = OpNode n

p_tyOpTree ::
  -- | Binary OpTree to render, enhanced with information regarding operator fixity and n-ary tree context
  OpTree (LHsType GhcPs) (OpSubTreeInfo (LHsType GhcPs) (LocatedN RdrName)) ->
  R ()
p_tyOpTree (OpNode n) = located n p_hsType
p_tyOpTree (OpBranch x OpSubTreeInfo {..} y) = do
  let placement = opBranchPlacement stAllNodesStartingSameLine stLastNode tyOpPlacement x y
      p_op = p_rdrName opOp
      p_x = p_tyOpTree x
      p_y = p_tyOpTree y
  ub <- opBranchBraceStyle placement
  -- The OpTree inherits the layout of the n-ary subtree it was part of. This ensures that a globally multiline "+" chain won't allow multiple additions on a single line
  switchLayout [stSpan] $ do
    ub p_x
    -- An indentation bump for the op + RHS is only required when we are on the left edge of the global OpTree, because an indent has not been produced yet by a parent context
    placeHanging' opOnBinaryTreeLeftEdge placement $ do
      p_op
      space
      p_y

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import GHC.Hs
import GHC.Types.Fixity
import GHC.Types.Name (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc
import Ormolu.Parser.CommentStream (LComment)
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common (p_rdrName)
import Ormolu.Printer.Meat.Declaration.Value
  ( IsApplicand (..),
    cmdTopPlacement,
    exprPlacement,
    p_hsCmdTop,
    p_hsExpr,
    p_hsExpr',
  )
import Ormolu.Printer.Meat.Type (p_hsType)
import Ormolu.Printer.Operators

-- | Extract the operator name of the specified 'HsExpr' if this expression
-- corresponds to an operator.
getOpName :: HsExpr GhcPs -> Maybe RdrName
getOpName = \case
  HsVar _ (L _ a) -> Just a
  _ -> Nothing

-- | Convert an operator name to a 'String'.
getOpNameStr :: RdrName -> String
getOpNameStr = occNameString . rdrNameOcc

-- | Decide whether the operands of an operator chain should be hanging.
opBranchPlacement ::
  (HasLoc l) =>
  -- | Placer function for nodes
  (ty -> Placement) ->
  -- | First expression of the chain
  OpTree (GenLocated l ty) op ->
  -- | Last expression of the chain
  OpTree (GenLocated l ty) op ->
  Placement
opBranchPlacement placer firstExpr lastExpr
  -- If the start of the first argument and the start of the last argument
  -- are on the same line, and the last argument has a hanging form, use
  -- hanging placement.
  | isOneLineSpan
      ( mkSrcSpan
          (srcSpanStart (opTreeLoc firstExpr))
          (srcSpanStart (opTreeLoc lastExpr))
      ),
    OpNode (L _ n) <- lastExpr =
      placer n
  | otherwise = Normal

-- | Decide whether or not to use braces based on the layout and placement
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
exprOpTree (L _ (OpApp _ x op y)) = BinaryOpBranches (exprOpTree x) op (exprOpTree y)
exprOpTree n = OpNode n

-- | Print an operator tree where leaves are values.
p_exprOpTree ::
  -- | Bracket style to use
  BracketStyle ->
  -- | N-ary 'OpTree' to render, enhanced with information regarding
  -- operator fixity
  OpTree (LHsExpr GhcPs) (OpInfo (LHsExpr GhcPs)) ->
  R ()
p_exprOpTree s (OpNode x) = located x (p_hsExpr' NotApplicand s)
p_exprOpTree s t@(OpBranches exprs@(firstExpr :| otherExprs) ops) = do
  let placement =
        opBranchPlacement
          exprPlacement
          firstExpr
          (last otherExprs)
      rightMostNode = \case
        n@(OpNode _) -> n
        OpBranches exprs'' _ -> rightMostNode (NE.last exprs'')
      isDoBlock = \case
        OpNode (L _ (HsDo _ ctx _)) -> case ctx of
          DoExpr _ -> True
          MDoExpr _ -> True
          _ -> False
        _ -> False
      -- Whether we could place the operator in a trailing position,
      -- followed by a breakpoint before the RHS
      couldBeTrailing (prevExpr, opi) =
        -- An operator with fixity InfixR 0, like seq, $, and the $ variants,
        -- is required.
        isHardSplitterOp (opiFixityApproximation opi)
          -- The LHS must be single-line.
          && isOneLineSpan (opTreeLoc prevExpr)
          -- This can only happen when a breakpoint would have been added
          -- anyway.
          && placement == Normal
          -- If the node just to the left of the operator (that is, the
          -- rightmost node of the subtree prevExpr) is a do-block, then we
          -- cannot place the operator in a trailing position, because it
          -- would be read as being part of the do-block.
          && not (isDoBlock $ rightMostNode prevExpr)
      -- A staircase of two or more trailing operators is only worthwhile when
      -- the operand at the very end of the chain has a hanging form (a do
      -- block, lambda, case, etc.): the trailing operator then introduces that
      -- block. When such a chain ends in an ordinary expression (a variable,
      -- literal, or plain application) the trailing layout only produces
      -- ever-deepening indentation, so we fall back to the leading-operator
      -- layout. A single hard splitter is exempt: it does not form a pyramid
      -- and trailing is the idiomatic way to introduce its operand.
      chainEndsInHangingForm =
        case rightMostNode t of
          OpNode (L _ n) -> exprPlacement n == Hanging
          _ -> False
      isSingleOperator = case ops of
        [_] -> True
        _ -> False
  -- A comment written on its own line in front of an operator forces the
  -- operator onto a line of its own. In trailing position that line starts
  -- at the indentation of the statement, and @$@ at the start of a line in
  -- a @do@ block is read as a new statement rather than as a continuation.
  -- The leading layout indents instead, so it keeps the meaning.
  opsAreCommented <-
    or <$> traverse (fmap (not . null) . leadingComments . opiOp) ops
  -- If all operators at the current level match the conditions to be
  -- trailing, and the chain is either a single operator or ends in a
  -- hanging form, then put the operators in a trailing position.
  let isTrailing =
        (isSingleOperator || chainEndsInHangingForm)
          && not opsAreCommented
          && all couldBeTrailing (zip (NE.toList exprs) ops)
  ub <- if isTrailing then return useBraces else opBranchBraceStyle placement
  let p_x = ub $ p_exprOpTree s firstExpr
      putOpsExprs prevExpr (opi : ops') (expr : exprs') = do
        let isLast = null exprs'
            ub' = if not isLast then ub else id
            p_op = located (opiOp opi) p_hsExpr
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

-- | The comments that will be printed in front of a located thing.
leadingComments :: (HasLoc l) => GenLocated l a -> R [LComment]
leadingComments (L l _) = case locA l of
  RealSrcSpan spn _ -> getCommentsBefore spn
  _ -> pure []

-- | Convert a 'LHsCmdTop' containing an operator tree to the 'OpTree'
-- intermediate representation.
cmdOpTree :: LHsCmdTop GhcPs -> OpTree (LHsCmdTop GhcPs) (LHsExpr GhcPs)
cmdOpTree = \case
  (L _ (HsCmdTop _ (L _ (HsCmdArrForm _ op Infix [x, y])))) ->
    BinaryOpBranches (cmdOpTree x) op (cmdOpTree y)
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
p_cmdOpTree s t@(OpBranches (firstExpr :| otherExprs) ops) = do
  let placement =
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

-- | Check whether the given expression has a hanging form. Added for
-- symmetry with 'exprPlacement' and 'cmdTopPlacement', all of which are used
-- in the @p_xxxOpTree@ functions together with 'opBranchPlacement'.
tyOpPlacement :: HsType GhcPs -> Placement
tyOpPlacement = \case
  _ -> Normal

-- | Convert an 'LHsType' containing an operator tree to the 'OpTree'
-- intermediate representation.
tyOpTree :: LHsType GhcPs -> OpTree (LHsType GhcPs) (LocatedN RdrName)
tyOpTree (L _ (HsOpTy _ _ l op r)) =
  BinaryOpBranches (tyOpTree l) op (tyOpTree r)
tyOpTree n = OpNode n

-- | Print an operator tree where leaves are types.
p_tyOpTree ::
  -- | N-ary 'OpTree' to render, enhanced with information regarding
  -- operator fixity
  OpTree (LHsType GhcPs) (OpInfo (LocatedN RdrName)) ->
  R ()
p_tyOpTree (OpNode n) = located n p_hsType
p_tyOpTree t@(OpBranches (firstExpr :| otherExprs) ops) = do
  let placement =
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

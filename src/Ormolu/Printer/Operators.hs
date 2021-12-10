{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module helps handle operator chains composed of different
-- operators that may have different precedence and fixities.
module Ormolu.Printer.Operators
  ( OpTree (..),
    OpInfo (..),
    opTreeLoc,
    reassociateOpTree,
    isHardSplitterOp,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Fixity
import Ormolu.Utils

-- | Intermediate representation of operator trees, where a branching is not
-- just a binary branching (with a left node, right node, and operator like
-- in OpTree), but rather a n-ary branching, with n + 1 nodes and n
-- operators (n >= 1).
--
-- This representation allows us to put all the operators with the same
-- precedence level as direct siblings in this tree, to better represent the
-- idea of a chain of operators.
data OpTree ty op
  = -- | A node which is not an operator application
    OpNode ty
  | -- | A subtree of operator application(s). The invariant is: @length
    -- noptExprs == length noptOpts + 1@. @OpBranch { noptExprs = [x, y, z],
    -- noptOps = [op1, op2] }@ represents the expression @x op1 y op2 z@.
    OpBranches [OpTree ty op] [op]
  deriving (Eq, Show)

-- | Wrapper for an operator, carrying information about its name and
-- fixity.
data OpInfo op = OpInfo
  { -- | The actual operator
    opiOp :: op,
    -- | Its name, if available. We use 'Maybe String' here instead of
    -- 'String' because the name-fetching function received by
    -- 'reassociateOpTree' returns a 'Maybe'
    opiName :: Maybe String,
    -- | Information about the fixity direction and precedence level of the
    -- operator
    opiFix :: FixityInfo
  }
  deriving (Eq)

-- | Compare the precedence level of two operators. 'OpInfo' is required
-- (and not just 'FixityInfo') because operator names are used in the case
-- of equality.
compareOp :: OpInfo op -> OpInfo op -> Maybe Ordering
compareOp
  (OpInfo _ mName1 FixityInfo {fiMinPrecedence = min1, fiMaxPrecedence = max1})
  (OpInfo _ mName2 FixityInfo {fiMinPrecedence = min2, fiMaxPrecedence = max2}) =
    if
        -- Only declare two precedence levels as equal when
        --  * either both precedence levels are precise
        --    (fixMinPrec == fixMaxPrec) and match
        --  * or when the precedence levels are imprecise but when the
        --    operator names match
        | min1 == min2
            && max1 == max2
            && (min1 == max1 || sameSymbol) ->
            Just EQ
        | max1 < min2 -> Just LT
        | max2 < min1 -> Just GT
        | otherwise -> Nothing
    where
      sameSymbol = case (mName1, mName2) of
        (Just n1, Just n2) -> n1 == n2
        _ -> False

-- | Return combined 'SrcSpan's of all elements in this 'OpTree'.
opTreeLoc :: HasSrcSpan l => OpTree (GenLocated l a) b -> SrcSpan
opTreeLoc (OpNode n) = getLoc' n
opTreeLoc (OpBranches exprs _) =
  combineSrcSpans' . NE.fromList . fmap opTreeLoc $ exprs

-- | Re-associate an 'OpTree' taking into account precedence of operators.
-- Users are expected to first construct an initial 'OpTree', then
-- re-associate it using this function before printing.
reassociateOpTree ::
  (HasSrcSpan l, HasSrcSpan l') =>
  -- | How to get name of an operator
  (op -> Maybe RdrName) ->
  -- | Fixity Map
  FixityMap ->
  -- | Original 'OpTree'
  OpTree (GenLocated l ty) (GenLocated l' op) ->
  -- | Re-associated 'OpTree', with added context and info around operators
  OpTree (GenLocated l ty) (OpInfo (GenLocated l' op))
reassociateOpTree getOpName fixityMap =
  reassociateFlatOpTree
    . makeFlatOpTree
    . addFixityInfo fixityMap (getOpName . unLoc)

-- | Wrap each operator of the tree with the 'OpInfo' struct, to carry the
-- information about its fixity (extracted from the specified fixity map).
addFixityInfo ::
  -- | Fixity map for operators
  FixityMap ->
  -- | How to get the name of an operator
  (op -> Maybe RdrName) ->
  -- | 'OpTree'
  OpTree ty op ->
  -- | 'OpTree', with fixity info wrapped around each operator
  OpTree ty (OpInfo op)
addFixityInfo _ _ (OpNode n) = OpNode n
addFixityInfo fixityMap getOpName (OpBranches exprs ops) =
  OpBranches
    (addFixityInfo fixityMap getOpName <$> exprs)
    (toOpInfo <$> ops)
  where
    toOpInfo o = OpInfo o mName fixityInfo
      where
        mName = occNameString . rdrNameOcc <$> getOpName o
        fixityInfo =
          fromMaybe
            defaultFixityInfo
            (mName >>= flip Map.lookup fixityMap)

-- | Given a 'OpTree' of any shape, produce a flat 'OpTree', where every
-- node and operator is directly connected to the root.
makeFlatOpTree :: OpTree ty op -> OpTree ty op
makeFlatOpTree (OpNode n) = OpNode n
makeFlatOpTree (OpBranches exprs ops) =
  OpBranches rExprs rOps
  where
    makeFlatOpTree' expr = case makeFlatOpTree expr of
      OpNode n -> ([OpNode n], [])
      OpBranches noptExprs noptOps -> (noptExprs, noptOps)
    flattenedSubTrees = makeFlatOpTree' <$> exprs
    rExprs = concatMap fst flattenedSubTrees
    rOps = concat $ interleave (snd <$> flattenedSubTrees) (pure <$> ops)
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    interleave [] ys = ys
    interleave xs [] = xs

-- | Starting from a flat 'OpTree' (i.e. a n-ary tree of depth 1,
-- without regard for operator fixities), build an 'OpTree' with proper
-- sub-trees (according to the fixity info carried by the nodes).
--
-- We have two complementary ways to build the proper sub-trees:
--
-- * if we can find operator(s) minOps at the current level where
--     forall (op1, op2) \in minOps x minOps, op1 `equal` op2
--     forall (op1, op2) \in minOps x (opsOfCurrentLevel \ minOps),
--       op1 `lessThan` op2
--   then we can build a subtree with the exprs and ops located "between"
--   each element of minOps.
--   For example, if minOps = {op0, op2, op5},
--   and if [...] means "extract a subtree", then
--   currentLevel =
--     [ex0 op0 ex1 op1 ex2 op2 ex3 op3 ex4 op4 ex5 op5 ex6 op6 ex7]
--   will become
--     [ex0 op0 [ex1 op1 ex2] op2 [ex3 op3 ex4 op4 ex5] op5 [ex6 op6 ex7]]
-- * if we can find operator(s) maxOps at the current level where
--     forall (op1, op2) \in maxOps x maxOps, op1 `equal` op2
--     forall (op1, op2) \in maxOps x (opsOfCurrentLevel \ maxOps),
--       op1 `greaterThan` op2
--   then we can build a subtree with every contiguous range of elements
--   from maxOps (and the exprs on their sides)
--   For example, if maxOps = {op0, op1, op4},
--   and if [...] means "extract a subtree", then
--   currentLevel =
--     [ex0 op0 ex1 op1 ex2 op2 ex3 op3 ex4 op4 ex5 op5 ex6 op6 ex7]
--   will become
--     [[ex0 op0 ex1 op1 ex2] op2 ex3 op3 [ex4 op4 ex5] op5 ex6 op6 ex7]
--
-- We will also recursively apply the same logic on every sub-tree built
-- during the process. The two principles are not overlapping and thus are
-- required, because we are comparing precedence level ranges. In the case
-- where we can't find a non-empty set {min,max}Ops with one logic or the
-- other, we finally try to split the tree on “hard splitters” if there is
-- any.
reassociateFlatOpTree ::
  -- | Flat 'OpTree'
  OpTree ty (OpInfo op) ->
  -- | Re-associated 'OpTree'
  OpTree ty (OpInfo op)
reassociateFlatOpTree tree@(OpNode _) = tree
reassociateFlatOpTree tree@(OpBranches noptExprs noptOps) =
  case indexOfMinMaxPrecOps noptOps of
    (Just minIndices, _) -> splitTree noptExprs noptOps minIndices
    (_, Just maxIndices) -> groupTree noptExprs noptOps maxIndices
    _ -> case indicesOfHardSplitter of
      [] -> tree
      indices -> splitTree noptExprs noptOps indices
  where
    indicesOfHardSplitter =
      fmap fst $
        filter (isHardSplitterOp . opiFix . snd) $
          zip [0 ..] noptOps
    indexOfMinMaxPrecOps [] = (Nothing, Nothing)
    indexOfMinMaxPrecOps (oo : oos) = go oos 1 oo (Just [0]) oo (Just [0])
      where
        go ::
          -- Remaining operators to look up
          [OpInfo op] ->
          -- Index of the next operator
          Int ->
          -- representative of the current minOps set, if there is one,
          -- or representative of the lowest precedence level encountered
          -- so far otherwise
          OpInfo op ->
          -- indices of the elements of the candidate minOps set,
          -- if there is any
          Maybe [Int] ->
          -- representative of the current maxOps set, if there is one, or
          -- representative of the highest precedence level encountered
          -- so far otherwise
          OpInfo op ->
          -- indices of the elements of the candidate maxOps set,
          -- if there is any
          Maybe [Int] ->
          -- (indices of minOps elements, indices of maxOps elements)
          (Maybe [Int], Maybe [Int])
        go [] _ _ minRes _ maxRes = (reverse <$> minRes, reverse <$> maxRes)
        go (o : os) i minOpi minRes maxOpi maxRes =
          let (minOpi', minRes') = case compareOp o minOpi of
                Just EQ -> (minOpi, (:) i <$> minRes)
                Just LT -> (o, Just [i])
                Just GT -> (minOpi, minRes)
                Nothing -> (combine minOpi o, Nothing)
              (maxOpi', maxRes') = case compareOp o maxOpi of
                Just EQ -> (maxOpi, (:) i <$> maxRes)
                Just LT -> (maxOpi, maxRes)
                Just GT -> (o, Just [i])
                Nothing -> (combine maxOpi o, Nothing)
              -- Merge two potential {min/max}Ops representatives for
              -- which the comparison gave 'OpUnknown' into a representative
              -- of the {lowest/highest} precedence level encountered so far
              combine (OpInfo x _ fix1) (OpInfo _ _ fix2) =
                OpInfo x Nothing (fix1 <> fix2)
           in go os (i + 1) minOpi' minRes' maxOpi' maxRes'
    -- If indices = [0, 2, 5], transform
    --   [ex0 op0 ex1 op1 ex2 op2 ex3 op3 ex4 op4 ex5 op5 ex6 op6 ex7]
    -- into
    --   [ex0 op0 [ex1 op1 ex2] op2 [ex3 op3 ex4 op4 ex5] op5 [ex6 op6 ex7]]
    splitTree nExprs nOps indices = go nExprs nOps indices 0 [] [] [] []
      where
        go ::
          -- Remaining exprs to look up
          [OpTree ty (OpInfo op)] ->
          -- Remaining ops to look up
          [OpInfo op] ->
          -- Remaining list of indices of operators on which to split
          -- (sorted)
          [Int] ->
          -- Index of the next expr/op
          Int ->
          -- Bag for exprs for the subtree we are building
          [OpTree ty (OpInfo op)] ->
          -- Bag for ops for the subtree we are building
          [OpInfo op] ->
          -- Bag for exprs of the result tree
          [OpTree ty (OpInfo op)] ->
          -- Bag for ops of the result tree
          [OpInfo op] ->
          -- Result tree
          OpTree ty (OpInfo op)
        go [] _ _ _ subExprs subOps resExprs resOps =
          -- No expr left to process.
          -- because we are in a "splitting" logic, there is at least one
          -- expr in the subExprs bag, so we build a subtree (if necessary)
          -- with sub-bags, add the node/subtree to the result bag, and then
          -- emit the result tree
          let resExpr = buildFromSub subExprs subOps
           in OpBranches (reverse (resExpr : resExprs)) (reverse resOps)
        go (x : xs) (o : os) (idx : idxs) i subExprs subOps resExprs resOps
          | i == idx =
              -- The op we are looking at is one on which we need to split.
              -- So we build a subtree from the sub-bags and the current
              -- expr, append it to the result exprs, and continue with
              -- cleared sub-bags
              let resExpr = buildFromSub (x : subExprs) subOps
               in go xs os idxs (i + 1) [] [] (resExpr : resExprs) (o : resOps)
        go (x : xs) ops idxs i subExprs subOps resExprs resOps =
          -- Either there is no op left, or the op we are looking at is not
          -- one on which we need to split. So we just add both the current
          -- expr and current op (if there is any) to the sub-bags
          let (ops', subOps') = moveOneIfPossible ops subOps
           in go xs ops' idxs (i + 1) (x : subExprs) subOps' resExprs resOps

    -- If indices = [0, 1, 4], transform
    --   [ex0 op0 ex1 op1 ex2 op2 ex3 op3 ex4 op4 ex5 op5 ex6 op6 ex7]
    -- into
    --   [[ex0 op0 ex1 op1 ex2] op2 ex3 op3 [ex4 op4 ex5] op5 ex6 op6 ex7]
    groupTree nExprs nOps indices = go nExprs nOps indices 0 [] [] [] []
      where
        go ::
          -- remaining exprs to look up
          [OpTree ty (OpInfo op)] ->
          -- remaining ops to look up
          [OpInfo op] ->
          -- remaining list of indices of operators on which to group (sorted)
          [Int] ->
          -- index of the next expr/op
          Int ->
          -- bag for exprs for the subtree we are building
          [OpTree ty (OpInfo op)] ->
          -- bag for ops for the subtree we are building
          [OpInfo op] ->
          -- bag for exprs of the result tree
          [OpTree ty (OpInfo op)] ->
          -- bag for ops of the result tree
          [OpInfo op] ->
          -- result tree
          OpTree ty (OpInfo op)
        go [] _ _ _ subExprs subOps resExprs resOps =
          -- no expr left to process
          -- because we are in a "grouping" logic, the subExprs bag might be
          -- empty. If it is not, we build a subtree (if necessary) with
          -- sub-bags and add the resulting node/subtree to the result bag.
          -- In any case, we then emit the result tree
          let resExprs' =
                if null subExprs
                  then resExprs
                  else buildFromSub subExprs subOps : resExprs
           in OpBranches (reverse resExprs') (reverse resOps)
        go (x : xs) (o : os) (idx : idxs) i subExprs subOps resExprs resOps
          | i == idx =
              -- The op we are looking at is one on which we need to group.
              -- So we just add the current expr and op to the sub-bags.
              go xs os idxs (i + 1) (x : subExprs) (o : subOps) resExprs resOps
        go (x : xs) ops idxs i subExprs@(_ : _) subOps resExprs resOps =
          -- Either there is no op left, or the op we are looking at is not
          -- one on which we need to split, but in any case the sub-bags are
          -- not empty. So we finalize the started group using sub-bags and
          -- the current expr, to form a subtree which is then added to the
          -- result bag.
          let (ops', resOps') = moveOneIfPossible ops resOps
              resExpr = buildFromSub (x : subExprs) subOps
           in go xs ops' idxs (i + 1) [] [] (resExpr : resExprs) resOps'
        go (x : xs) ops idxs i [] subOps resExprs resOps =
          -- Either there is no op left, or the op we are looking at is not
          -- one on which we need to split, but the sub-bags are empty. So
          -- we just add both the current expr and current op (if there is
          -- any) to the result bags
          let (ops', resOps') = moveOneIfPossible ops resOps
           in go xs ops' idxs (i + 1) [] subOps (x : resExprs) resOps'

    moveOneIfPossible [] bs = ([], bs)
    moveOneIfPossible (a : as) bs = (as, a : bs)

    buildFromSub subExprs subOps = reassociateFlatOpTree $ case subExprs of
      -- Do not build a subtree when the potential subtree would have
      -- 1 expr(s) and 0 op(s)
      [x] -> x
      _ -> OpBranches (reverse subExprs) (reverse subOps)

-- | Indicate if an operator has @'InfixR' 0@ fixity. We special-case this
-- class of operators because they often have, like ('$'), a specific
-- “separator” use-case, and we sometimes format them differently than other
-- operators.
isHardSplitterOp :: FixityInfo -> Bool
isHardSplitterOp = (== FixityInfo (Just InfixR) 0 0)

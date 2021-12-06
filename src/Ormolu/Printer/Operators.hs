{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module helps handle operator chains composed of different
-- operators that may have different precedence and fixities.
module Ormolu.Printer.Operators
  ( OpTree (..),
    OpInfo (..),
    SubTreeInfo (..),
    opTreeLoc,
    reassociateOpTree,
    isHardSplitterOp,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Fixity
import Ormolu.Utils

-- | Representation of operator trees when they are printed. It has two type
-- parameters: @ty@ is the type of sub-expressions, while @op@ is the type
-- of operators.
data OpTree ty op
  = -- | A node which is not an operator application
    OpNode ty
  | -- | A subtree of operator application(s)
    OpBranch
      (OpTree ty op)
      op
      (OpTree ty op)

-- | Intermediate representation of operator trees, where a branching is not
-- just a binary branching (with a left node, right node, and operator like
-- in OpTree), but rather a n-ary branching, with n + 1 nodes and n
-- operators (n >= 1).
--
-- This representation allows us to put all the operators with the same
-- precedence level as direct siblings in this tree, to better represent the
-- idea of a chain of operators. We then extract information relative to the
-- position of each operator in the n-ary tree, to get a more context-aware
-- formatting in @p_xxxOpTree@ in which the tree is in its binary form
-- again.
data NaryOpTree ty op
  = -- | A node which is not an operator application
    OpLeaf ty
  | -- | A subtree of operator application(s). The invariant is: @length
    -- noptExprs == length noptOpts + 1@. @OpBranch { noptExprs = [x, y, z],
    -- noptOps = [op1, op2] }@ represents the expression @x op1 y op2 z@.
    OpBranches [NaryOpTree ty op] [op]
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

-- | Sub-tree wrapper carrying information about the context where an
-- operator appears.
data SubTreeInfo ty op = SubTreeInfo
  { -- | The actual operator
    stiOp :: op,
    -- | Whether the node of this operator is located directly on the left
    -- edge of the full binary 'OpTree'. This information is needed to know
    -- when to indent the RHS during printing of the binary 'OpTree'
    stiOnBinaryTreeLeftEdge :: Bool,
    -- | Span of the n-ary sub-tree this operator belongs to
    stiSpan :: SrcSpan,
    -- | Whether all the exprs of n-ary sub-tree were starting on the same
    -- line in the original source file
    stiAllNodesStartingSameLine :: Bool,
    -- | The rightmost sibling of the operator in n-ary sub-tree, if this
    -- sibling is not a subtree itself
    stiLastNode :: Maybe ty
  }

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
opTreeLoc (OpBranch l _ r) = combineSrcSpans (opTreeLoc l) (opTreeLoc r)

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
  OpTree (GenLocated l ty) (SubTreeInfo (GenLocated l ty) (OpInfo (GenLocated l' op)))
reassociateOpTree getOpName fixityMap =
  markLeftEdge
    . reassociateBinOpTree
    . setSubTreeInfo
    . reassociateFlatNaryOpTree
    . makeFlatNaryOpTree
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
addFixityInfo fixityMap getOpName (OpBranch x op y) =
  OpBranch
    (addFixityInfo fixityMap getOpName x)
    (toOpInfo op)
    (addFixityInfo fixityMap getOpName y)
  where
    toOpInfo o = OpInfo o mName fixityInfo
      where
        mName = occNameString . rdrNameOcc <$> getOpName o
        fixityInfo =
          fromMaybe
            defaultFixityInfo
            (mName >>= flip Map.lookup fixityMap)

-- | Given a binary 'OpTree', produce a flat 'NaryOpTree', where every node
-- and operator is directly connected to the root.
makeFlatNaryOpTree :: OpTree ty op -> NaryOpTree ty op
makeFlatNaryOpTree (OpNode n) = OpLeaf n
makeFlatNaryOpTree (OpBranch x op y) =
  OpBranches (xExprs ++ yExprs) (xOps ++ [op] ++ yOps)
  where
    (xExprs, xOps) = case makeFlatNaryOpTree x of
      OpLeaf n -> ([OpLeaf n], [])
      OpBranches noptExprs noptOps -> (noptExprs, noptOps)
    (yExprs, yOps) = case makeFlatNaryOpTree y of
      OpLeaf n -> ([OpLeaf n], [])
      OpBranches noptExprs noptOps -> (noptExprs, noptOps)

-- | Starting from a flat 'NaryOpTree' (i.e. a n-ary tree of depth 1,
-- without regard for operator fixities), build an 'NaryOpTree' with proper
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
reassociateFlatNaryOpTree ::
  -- | Flat 'NaryOpTree'
  NaryOpTree ty (OpInfo op) ->
  -- | Re-associated 'NaryOpTree'
  NaryOpTree ty (OpInfo op)
reassociateFlatNaryOpTree tree@(OpLeaf _) = tree
reassociateFlatNaryOpTree tree@(OpBranches noptExprs noptOps) =
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
          [NaryOpTree ty (OpInfo op)] ->
          -- Remaining ops to look up
          [OpInfo op] ->
          -- Remaining list of indices of operators on which to split
          -- (sorted)
          [Int] ->
          -- Index of the next expr/op
          Int ->
          -- Bag for exprs for the subtree we are building
          [NaryOpTree ty (OpInfo op)] ->
          -- Bag for ops for the subtree we are building
          [OpInfo op] ->
          -- Bag for exprs of the result tree
          [NaryOpTree ty (OpInfo op)] ->
          -- Bag for ops of the result tree
          [OpInfo op] ->
          -- Result tree
          NaryOpTree ty (OpInfo op)
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
          [NaryOpTree ty (OpInfo op)] ->
          -- remaining ops to look up
          [OpInfo op] ->
          -- remaining list of indices of operators on which to group (sorted)
          [Int] ->
          -- index of the next expr/op
          Int ->
          -- bag for exprs for the subtree we are building
          [NaryOpTree ty (OpInfo op)] ->
          -- bag for ops for the subtree we are building
          [OpInfo op] ->
          -- bag for exprs of the result tree
          [NaryOpTree ty (OpInfo op)] ->
          -- bag for ops of the result tree
          [OpInfo op] ->
          -- result tree
          NaryOpTree ty (OpInfo op)
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

    buildFromSub subExprs subOps = reassociateFlatNaryOpTree $ case subExprs of
      -- Do not build a subtree when the potential subtree would have
      -- 1 expr(s) and 0 op(s)
      [x] -> x
      _ -> OpBranches (reverse subExprs) (reverse subOps)

-- | Add information about binary and n-ary subtree context to every
-- operator of a reassociated 'NaryOpTree'. This function cannot set the
-- 'opOnBinaryTreeLeftEdge' field to the proper value, so it will default to
-- 'False', and needs to be updated later.
setSubTreeInfo ::
  HasSrcSpan l =>
  NaryOpTree (GenLocated l ty) (OpInfo op) ->
  NaryOpTree (GenLocated l ty) (SubTreeInfo (GenLocated l ty) (OpInfo op))
setSubTreeInfo = go
  where
    go (OpLeaf x) = OpLeaf x
    go t@(OpBranches noptExprs noptOps) =
      OpBranches noptExprs' (setSubTreeInfo' <$> noptOps)
      where
        noptExprs' = go <$> noptExprs
        stSpan = naryOpTreeLoc t
        allStartingSameLine =
          isOneLineSpan
            ( mkSrcSpan
                (srcSpanStart . naryOpTreeLoc . head $ noptExprs)
                (srcSpanStart . naryOpTreeLoc . last $ noptExprs)
            )
        setSubTreeInfo' op =
          SubTreeInfo
            { stiOp = op,
              stiOnBinaryTreeLeftEdge = False,
              stiSpan = stSpan,
              stiAllNodesStartingSameLine = allStartingSameLine,
              stiLastNode = case last noptExprs' of
                OpLeaf leaf -> Just leaf
                _ -> Nothing
            }
        naryOpTreeLoc (OpLeaf leaf) = getLoc' leaf
        naryOpTreeLoc (OpBranches exprs _) =
          combineSrcSpans' . NE.fromList $ naryOpTreeLoc <$> exprs

-- | Transform a reassociated 'NaryOpTree' into an 'OpTree' with sub-tree
-- context and fixity info, using fixity direction of the operators.
reassociateBinOpTree ::
  NaryOpTree ty (SubTreeInfo ty (OpInfo op)) ->
  OpTree ty (SubTreeInfo ty (OpInfo op))
reassociateBinOpTree (OpLeaf n) = OpNode n
reassociateBinOpTree (OpBranches noptExprs noptOps) =
  case fiDirection of
    Just InfixR -> reassociateRight (reassociateBinOpTree <$> noptExprs) noptOps
    _ -> reassociateLeft (reassociateBinOpTree <$> noptExprs) noptOps
  where
    FixityInfo {fiDirection} = sconcat $ NE.fromList (opiFix . stiOp <$> noptOps)
    reassociateRight (x : xs) (o : os) =
      OpBranch x o (reassociateRight xs os)
    reassociateRight [x] [] = x
    reassociateRight _ _ = error "Invalid case"
    reassociateLeft (x : y : xs) (o : os) =
      reassociateLeft (OpBranch x o y : xs) os
    reassociateLeft [x] [] = x
    reassociateLeft _ _ = error "Invalid case"

-- | Set the 'stiOnBinaryTreeLeftEdge' field in the 'SubTreeInfo' wrapper.
markLeftEdge ::
  OpTree ty (SubTreeInfo ty (OpInfo op)) ->
  OpTree ty (SubTreeInfo ty (OpInfo op))
markLeftEdge = \case
  OpNode n -> OpNode n
  OpBranch x op y ->
    OpBranch
      (markLeftEdge x)
      op {stiOnBinaryTreeLeftEdge = True}
      y

-- | Indicate if an operator has @'InfixR' 0@ fixity. We special-case this
-- class of operators because they often have, like ('$'), a specific
-- “separator” use-case, and we sometimes format them differently than other
-- operators.
isHardSplitterOp :: FixityInfo -> Bool
isHardSplitterOp = (== FixityInfo (Just InfixR) 0 0)

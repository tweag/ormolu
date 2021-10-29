{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module helps handle operator chains composed of different
-- operators that may have different precedence and fixities.
{-# LANGUAGE NamedFieldPuns #-}
module Ormolu.Printer.Operators
  ( OpTree (..),
    opTreeLoc,
    reassociateOpTree,
    OpFix (..),
    FixityInfo (..),
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Utils (combineSrcSpans')
import Ormolu.Printer.OperatorFixityMap (fixityMap)
import Ormolu.Printer.FixityInfo (FixityInfo (..), defaultFixityInfo)
import Data.List.NonEmpty (fromList)

-- | Intermediate representation of operator trees. It has two type
-- parameters: @ty@ is the type of sub-expressions, while @op@ is the type
-- of operators.
data OpTree ty op
  = OpNode ty
  | OpBranches
    { optrExprs :: [OpTree ty op],
      optrOps :: [op]
    }
  deriving (Eq, Show)

data OpFix op = OpFix op (Maybe String) FixityInfo
  deriving Eq

data FixityOrdering
  = FEqual
  | FLower
  | FGreater
  | FUnknown

compareOpf :: OpFix op -> OpFix op -> FixityOrdering
compareOpf (OpFix _ mName1 FixityInfo{fixMinPrec=min1, fixMaxPrec=max1}) (OpFix _ mName2 FixityInfo{fixMinPrec=min2,fixMaxPrec=max2}) =
  if
      | min1 == min2 && max1 == max2 && (min1 == max1 || sameSymbol) -> FEqual
      | max1 < min2 -> FLower
      | max2 < min1 -> FGreater
      | otherwise -> FUnknown
  where
    sameSymbol = case (mName1, mName2) of
      (Just n1, Just n2) -> n1 == n2
      _ -> False

-- | Return combined 'SrcSpan's of all elements in this 'OpTree'.
opTreeLoc :: OpTree (Located a) b -> SrcSpan
opTreeLoc (OpNode (L l _)) = l
opTreeLoc OpBranches{optrExprs} = combineSrcSpans' $ fromList (opTreeLoc <$> optrExprs)

-- | Re-associate an 'OpTree' taking into account automagically inferred
-- relative precedence of operators. Users are expected to first construct
-- an initial 'OpTree', then re-associate it using this function before
-- printing.
reassociateOpTree ::
  -- | How to get name of an operator
  (op -> Maybe RdrName) ->
  -- | Original 'OpTree'
  OpTree (Located ty) (Located op) ->
  -- | Re-associated 'OpTree'
  OpTree (Located ty) (OpFix (Located op))
reassociateOpTree getOpName opTree =
  reassociateNormOpTree normOpTree
  where
    normOpTree = normalizeOpTree treeWithFixity
    treeWithFixity = addFixityInfo fixityMap (getOpName . unLoc) opTree

addFixityInfo ::
  -- | Fixity map for operators
  Map String FixityInfo ->
  -- | How to get the name of an operator
  (op -> Maybe RdrName) ->
    -- | 'OpTree'
  OpTree ty op ->
  -- | 'OpTree', with fixity info
  OpTree ty (OpFix op)
addFixityInfo _ _ (OpNode n) = OpNode n
addFixityInfo fixityMap getOpName OpBranches{optrExprs, optrOps} =
  OpBranches
    { optrExprs = addFixityInfo fixityMap getOpName <$> optrExprs,
      optrOps = toOpFix <$> optrOps
    }
  where
    toOpFix op = OpFix op mName fixityInfo where
      mName = occNameString . rdrNameOcc <$> getOpName op
      fixityInfo = fromMaybe defaultFixityInfo (mName >>= flip M.lookup fixityMap)

-- | Re-associate an 'OpTree' given the map with operator fixities.
reassociateNormOpTree ::
  -- | Normalized OpTree with fixity Info
  OpTree ty (OpFix op) ->
  -- | Re-associated 'OpTree', with fixity info
  OpTree ty (OpFix op)
reassociateNormOpTree tree@(OpNode _) = tree
reassociateNormOpTree tree@OpBranches{optrExprs, optrOps} = case indexOfMinMaxPrecOps optrOps of
  (Just minIndices, _) -> splitTree optrExprs optrOps minIndices
  (_, Just maxIndices) -> groupTree optrExprs optrOps maxIndices
  _ -> tree
  where
    indexOfMinMaxPrecOps [] = (Nothing, Nothing)
    indexOfMinMaxPrecOps (o:os) = go os 1 o (Just [0]) o (Just [0]) where
      go [] _ _ minRes _ maxRes = (reverse <$> minRes, reverse <$> maxRes)
      go (o:os) i minOpf minRes maxOpf maxRes =
        let (minOpf', minRes') = case compareOpf o minOpf of
              FEqual -> (minOpf, (:) i <$> minRes)
              FLower -> (o, Just [i])
              FGreater -> (minOpf, minRes)
              FUnknown -> (combine minOpf o, Nothing)
            (maxOpf', maxRes') = case compareOpf o maxOpf of
              FEqual -> (maxOpf, (:) i <$> maxRes)
              FLower -> (maxOpf, maxRes)
              FGreater -> (o, Just [i])
              FUnknown -> (combine maxOpf o, Nothing)
            combine (OpFix x mn1 fix1) (OpFix _ mn2 fix2) =
              OpFix x (Just $ fromMaybe "<unknown>" mn1 ++ "|" ++ fromMaybe "<unknown>" mn2) (fix1 <> fix2)
        in
        go os (i + 1) minOpf' minRes' maxOpf' maxRes'
    splitTree optrExprs optrOps indices = go optrExprs optrOps indices 0 [] [] [] [] where
      go [] _ _ _ subExprs subOps resExprs resOps =
        let resExpr = buildFromSub subExprs subOps in
        OpBranches {optrExprs=reverse (resExpr:resExprs), optrOps=reverse resOps}
      go (x:xs) (o:os) (idx:idxs) i subExprs subOps resExprs resOps | i == idx =
        let resExpr = buildFromSub (x:subExprs) subOps in
        go xs os idxs (i + 1) [] [] (resExpr : resExprs) (o : resOps)
      go (x:xs) ops (idx:idxs) i subExprs subOps resExprs resOps =
        let (ops', subOps') = moveOne ops subOps in
        go xs ops' idxs (i + 1) (x:subExprs) subOps' resExprs resOps
    groupTree optrExprs optrOps indices = go optrExprs optrOps indices 0 [] [] [] [] where
      go [] _ _ _ subExprs subOps resExprs resOps =
        let resExprs' = if null subExprs then resExprs else buildFromSub subExprs subOps:resExprs in
        OpBranches {optrExprs=reverse resExprs', optrOps=reverse resOps}
      go (x:xs) (o:os) (idx:idxs) i subExprs subOps resExprs resOps | i == idx =
        go xs os idxs (i + 1) (x:subExprs) (o:subOps) resExprs resOps
      go (x:xs) ops idxs i subExprs@(_:_) subOps resExprs resOps =
        let (ops', resOps') = moveOne ops resOps
            resExpr = buildFromSub subExprs subOps in
        go xs ops' idxs (i + 1) [] [] (resExpr : resExprs) resOps'
    moveOne [] bs = ([], bs)
    moveOne (a:as) bs = (as, a:bs)
    buildFromSub [x] [] = reassociateNormOpTree x
    buildFromSub subExprs subOps = reassociateNormOpTree OpBranches{optrExprs=reverse subExprs, optrOps=reverse subOps}

-- | Transform an 'OpTree' to put all operators at the same level
normalizeOpTree :: OpTree ty op -> OpTree ty op
normalizeOpTree (OpNode n) =
  OpNode n
normalizeOpTree OpBranches{optrExprs, optrOps} =
  OpBranches{optrExprs=optrExprs', optrOps=optrOps'} where
    (optrExprs', optrOps') = go optrExprs optrOps [] []
    go [] _ accExprs accOps = (reverse accExprs, reverse accOps)
    go (x:xs) ops accExprs accOps =
      let (ops', accOps') = moveOne ops accOps in
      case x of
        OpNode _ -> go xs ops' (x:accExprs) accOps'
        OpBranches{optrExprs, optrOps} ->
          let (innerExprs, innerOps) = go optrExprs optrOps [] [] in
          go xs ops' (innerExprs ++ accExprs) (innerOps ++ accOps')
    moveOne (o:os) acc = (os, o:acc)
    moveOne [] acc = ([], acc)
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
  )
where

import Data.Function (on)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Types.Basic
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Utils (unSrcSpan)

-- | Intermediate representation of operator trees. It has two type
-- parameters: @ty@ is the type of sub-expressions, while @op@ is the type
-- of operators.
data OpTree ty op
  = OpNode ty
  | OpBranches
    { optrExprs :: [OpTree ty op],
      optrOps :: [op]
    }

data OpFix op = OpFix op (Maybe String) FixityInfo

data Precedence
  = PrecUnique Int
  | PrecRange Int Int
  deriving (Eq, Show)

instance Semigroup Precedence where
  (PrecUnique a) <> (PrecUnique b)
    | a == b = PrecUnique a
    | otherwise = PrecRange (min a b) (max a b)
  PrecUnique a <> PrecRange min2 max2 = PrecRange (min a min2) (max a max2)
  PrecRange min1 max1 <> PrecUnique a = PrecRange (min min1 a) (max max1 a)
  PrecRange min1 max1 <> PrecRange min2 max2 = PrecRange (min min1 min2) (max max1 max2)

data FixityInfo = FixityInfo
  { fixDirection :: Maybe FixityDirection,
    fixMinPrec :: Int,
    fixMaxPrec :: Int
  }
defaultFixityInfo :: FixityInfo
defaultFixityInfo = FixityInfo
  { fixDirection = Nothing,
    fixMinPrec = 0,
    fixMaxPrec = 9
  }

instance Semigroup FixityInfo where
  FixityInfo{fixDirection=dir1, fixMinPrec=min1, fixMaxPrec=max1} <> FixityInfo{fixDirection=dir2, fixMinPrec=min2, fixMaxPrec=max2} =
    FixityInfo{fixDirection=dir', fixMinPrec=min min1 min2, fixMaxPrec=max max1 max2} where
      dir' = case (dir1, dir2) of
        (Just a, Just b) | a == b -> Just a
        _ -> Nothing

data FixityOrdering
  = FEqual
  | FLower
  | FGreater
  | FUnknown

compareOpf :: OpFix op -> OpFix op -> FixityOrdering
compareOpf opf1@(OpFix _ mName1 f1@FixityInfo{fixMinPrec=min1, fixMaxPrec=max1}) opf2@(OpFix _ mName2 f2@FixityInfo{fixMinPrec=min2,fixMaxPrec=max2}) =
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
  OpTree (Located ty) (Located op)
reassociateOpTree getOpName opTree =
  reassociateNormOpTree $ normalizeOpTree treeWithFixity
  where
    treeWithFixity = addFixityNameInfo (buildFixityMap getOpName normOpTree) (getOpName . unLoc) opTree

addFixityInfo ::
  -- | Fixity map for operators
  Map String FixityInfo ->
  -- | How to get the name of an operator
  (op -> Maybe RdrName) ->
    -- | 'OpTree'
  OpTree ty op ->
  -- | 'OpTree', with fixity info
  OpTree ty (OpFix op)
addFixityInfo fixityMap getOpName (OpNode n) = OpNode n
addFixityInfo fixityMap getOpName OpBranches{optrExprs, optrOps} =
  OpBranches
    { optrExprs = addFixityInfo fixityMap getOpName <$> optrExprs,
      optrOps = toOpFix <$> optrOps
    }
  where
    toOpFix :: op -> OpFix op
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
reassociateNormOpTree tree@OpBranches{optrExprs, optrOps} = case indexOfLeastPrecOps optrOps of
  Just indices -> splitTree tree indices
  Nothing -> tree
  where
    indexOfLeastPrecOps [] = Nothing
    indexOfLeastPrecOps (o:os) = go os 1 o (Just [0]) where
      go [] _ _ res = reverse <$> res
      go (o:os) i minOpf res = case compareOpf o minOpf of
        FEqual -> go os (i + 1) minOpf ((:) i <$> res)
        FLower -> go os (i + 1) o (Just [i])
        FGreater -> go os (i + 1) minOpf res
        FUnknown ->
          let OpFix x mn1 fix1 = minOpf
              OpFix _ mn2 fix2 = o
              name = (fromMaybe "<unknown>" mn1) ++ "|" ++ (fromMaybe "<unknown>" mn2)
              combinedMin = OpFix x name (fix1 <> fix2) in
          go os (i + 1) combinedMin Nothing
    splitTree tree indices = go indices [] []


-- | A score assigned to an operator.
data Score
  = -- | The operator was placed at the beginning of a line
    AtBeginning Int
  | -- | The operator was placed at the end of a line
    AtEnd
  | -- | The operator was placed in between arguments on a single line
    InBetween
  deriving (Eq, Ord)

-- | Build a map of inferred 'Fixity's from an 'OpTree'.
buildFixityMap ::
  forall ty op.
  -- | How to get the name of an operator
  (op -> Maybe RdrName) ->
  -- | Operator tree
  OpTree (Located ty) (Located op) ->
  -- | Fixity map
  Map String Fixity
buildFixityMap getOpName opTree =
  addOverrides
    . M.fromList
    . concatMap (\(i, ns) -> map (\(n, _) -> (n, fixity i InfixL)) ns)
    . zip [2 ..]
    . L.groupBy ((==) `on` snd)
    . selectScores
    $ score opTree
  where
    addOverrides :: Map String Fixity -> Map String Fixity
    addOverrides m =
      M.fromList
        [ ("$", fixity 0 InfixR),
          (":", fixity 1 InfixR),
          (".", fixity 100 InfixL)
        ]
        `M.union` m
    fixity = Fixity NoSourceText
    score :: OpTree (Located ty) (Located op) -> [(String, Score)]
    score (OpNode _) = []
    score (OpBranch l o r) = fromMaybe (score r) $ do
      -- If we fail to get any of these, 'defaultFixity' will be used by
      -- 'reassociateOpTreeWith'.
      le <- srcSpanEndLine <$> unSrcSpan (opTreeLoc l) -- left end
      ob <- srcSpanStartLine <$> unSrcSpan (getLoc o) -- operator begin
      oe <- srcSpanEndLine <$> unSrcSpan (getLoc o) -- operator end
      rb <- srcSpanStartLine <$> unSrcSpan (opTreeLoc r) -- right begin
      oc <- srcSpanStartCol <$> unSrcSpan (getLoc o) -- operator column
      opName <- occNameString . rdrNameOcc <$> getOpName (unLoc o)
      let s
            | le < ob = AtBeginning oc
            | oe < rb = AtEnd
            | otherwise = InBetween
      return $ (opName, s) : score r
    selectScores :: [(String, Score)] -> [(String, Score)]
    selectScores =
      L.sortOn snd
        . mapMaybe
          ( \case
              [] -> Nothing
              xs@((n, _) : _) -> Just (n, selectScore $ map snd xs)
          )
        . L.groupBy ((==) `on` fst)
        . L.sort
    selectScore :: [Score] -> Score
    selectScore xs =
      case filter (/= InBetween) xs of
        [] -> InBetween
        xs' -> maximum xs'

----------------------------------------------------------------------------
-- Helpers

-- | Transform an 'OpTree' to put all operators at the same level
normalizeOpTree :: OpTree ty op -> OpTree ty op
normalizeOpTree (OpNode n) =
  OpNode n
normalizeOpTree OpBranches{optrExprs, optrOps} =
  OpBranches{optrExprs=optrExprs', optrOps=optrOps'} where
    (optrExprs', optrOps') = go optrExpr optrOps [] []
    go [] _ accExprs accOps = (reverse accExprs, reverse accOps)
    go (x:xs) ops accExprs accOps =
      let (ops', accOps') = moveOne ops accOps in
      case x of
        OpNode n -> go xs ops' (x:accExprs) accOps'
        OpBranches{optrExprs, optrOps} ->
          let (innerExprs, innerOps) = go optrExprs optrOps [] [] in
          go xs ops' (innerExprs ++ accExprs) (innerOps ++ accOps')
    moveOne (o:os) acc = (os, o:acc)
    moveOne [] acc = ([], acc)
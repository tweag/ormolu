{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module helps handle operator chains composed of different
-- operators that may have different precedence and fixities.

module Ormolu.Printer.Operators
  ( OpTree (..)
  , opTreeLoc
  , reassociateOpTree
  ) where

import BasicTypes (Fixity (..), SourceText (NoSourceText), defaultFixity, compareFixity)
import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)
import GHC
import OccName (mkVarOcc)
import RdrName (mkRdrUnqual)
import SrcLoc (combineSrcSpans)

-- | Intermediate representation of operator trees. It has two type
-- parameters: @ty@ is the type of sub-expressions, while @op@ is the type
-- of operators.

data OpTree ty op
  = OpNode ty
  | OpBranch
      (OpTree ty op)
      op
      (OpTree ty op)

-- | Return combined 'SrcSpan's of all elements in this 'OpTree'.

opTreeLoc :: OpTree (Located a) b -> SrcSpan
opTreeLoc (OpNode (L l _)) = l
opTreeLoc (OpBranch l _ r) = combineSrcSpans (opTreeLoc l) (opTreeLoc r)

-- | Re-associate an 'OpTree' taking into account automagically inferred
-- relative precedence of operators. Users are expected to first construct
-- an initial 'OpTree', then re-associate it using this function before
-- printing.

reassociateOpTree
  :: (op -> Maybe RdrName)      -- ^ How to get name of an operator
  -> OpTree (Located ty) (Located op) -- ^ Original 'OpTree'
  -> OpTree (Located ty) (Located op) -- ^ Re-associated 'OpTree'
reassociateOpTree getOpName opTree =
  reassociateOpTreeWith
    (buildFixityMap getOpName normOpTree)
    (getOpName . unLoc)
    normOpTree
  where
    normOpTree = normalizeOpTree opTree

-- | Re-associate an 'OpTree' given the map with operator fixities.

reassociateOpTreeWith
  :: forall ty op.
     [(RdrName, Fixity)]        -- ^ Fixity map for operators
  -> (op -> Maybe RdrName)      -- ^ How to get the name of an operator
  -> OpTree ty op               -- ^ Original 'OpTree'
  -> OpTree ty op               -- ^ Re-associated 'OpTree'
reassociateOpTreeWith fixityMap getOpName = go
  where
    fixityOf :: op -> Fixity
    fixityOf op = fromMaybe defaultFixity $ do
      opName <- getOpName op
      lookup opName fixityMap

    -- Here, left branch is already associated and the root alongside with
    -- the right branch is right-associated. This function picks up one item
    -- from the right and inserts it correctly to the left.
    --
    -- Also, we are using the 'compareFixity' function which returns if the
    -- expression should associate to right.
    go :: OpTree ty op -> OpTree ty op
    -- base cases
    go t@(OpNode _) = t
    go t@(OpBranch (OpNode _) _ (OpNode _)) = t
    -- shift one operator to the left at the beginning
    go (OpBranch l@(OpNode _) op (OpBranch l' op' r')) =
      go (OpBranch (OpBranch l op l') op' r')
    -- at the last operator, place the operator and don't recurse
    go (OpBranch (OpBranch l op r) op' r'@(OpNode _)) =
      if snd $ compareFixity (fixityOf op) (fixityOf op')
      then OpBranch l op (go $ OpBranch r op' r')
      else OpBranch (OpBranch l op r) op' r'
    -- else, shift one operator to left and recurse.
    go (OpBranch (OpBranch l op r) op' (OpBranch l' op'' r')) =
      if snd $ compareFixity (fixityOf op) (fixityOf op')
      then go $ OpBranch (OpBranch l op (go $ OpBranch r op' l')) op'' r'
      else go $ OpBranch (OpBranch (OpBranch l op r) op' l') op'' r'

-- | Build a map of inferred 'Fixity's from an 'OpTree'.

buildFixityMap
  :: forall ty op.
     (op -> Maybe RdrName)      -- ^ How to get the name of an operator
  -> OpTree (Located ty) (Located op) -- ^ Operator tree
  -> [(RdrName, Fixity)]               -- ^ Fixity map
buildFixityMap getOpName opTree =
  concatMap (\(i, ns) -> map (\(n, _) -> (n, fixity i InfixL)) ns)
    . zip [0..]
    . groupBy (doubleWithinEps 0.00001 `on` snd)
    . (overrides ++)
    . avgScores
    $ score opTree
  where
    -- Add a special case for ($), since it is pretty unlikely for someone
    -- to override it.
    overrides :: [(RdrName, Double)]
    overrides =
      [ (mkRdrUnqual $ mkVarOcc "$", -1)
      ]

    -- Assign scores to operators based on their location in the source.
    score :: OpTree (Located ty) (Located op) -> [(RdrName, Double)]
    score (OpNode _) = []
    score (OpBranch l o r) = fromMaybe (score r) $ do
      -- If we fail to get any of these, 'defaultFixity' will be used by
      -- 'reassociateOpTreeWith'.
      le <- srcSpanEndLine <$> unSrcSpan (opTreeLoc l) -- left end
      ob <- srcSpanStartLine <$> unSrcSpan (getLoc o) -- operator begin
      oe <- srcSpanEndLine <$> unSrcSpan (getLoc o) -- operator end
      rb <- srcSpanStartLine <$> unSrcSpan (opTreeLoc r) -- right begin
      oc <- srcSpanStartCol <$> unSrcSpan (getLoc o) -- operator column
      opName <- getOpName (unLoc o)

      let s =
           if le < ob
             -- if the operator is in the beginning of a line, assign
             -- a score relative to its column within range [0, 1).
             then fromIntegral oc / fromIntegral (maxCol + 1)
             -- if the operator is in the end of the line, assign the
             -- score 1.
             else
               if oe < rb
                 then 1
                 else 2 -- otherwise, assign a high score.
      return $ (opName, s) : score r

    avgScores :: [(RdrName, Double)] -> [(RdrName, Double)]
    avgScores
      = sortOn snd
        . map (\xs@((n, _):_) -> (n, avg $ map snd xs))
        . groupBy ((==) `on` fst)
        . sort

    avg :: [Double] -> Double
    avg i = sum i / fromIntegral (length i)

    -- The start column of the rightmost operator.
    maxCol = go opTree
      where
        go (OpNode (L _ _)) = 0
        go (OpBranch l (L o _) r) = maximum
          [ go l
          , maybe 0 srcSpanStartCol (unSrcSpan o)
          , go r
          ]

    unSrcSpan (RealSrcSpan r) = Just r
    unSrcSpan (UnhelpfulSpan _) = Nothing

----------------------------------------------------------------------------
-- Helpers

-- | Convert an 'OpTree' to with all operators having the same fixity and
-- associativity (left infix).

normalizeOpTree :: OpTree ty op -> OpTree ty op
normalizeOpTree (OpNode n)
  = OpNode n
normalizeOpTree (OpBranch (OpNode l) lop r)
  = OpBranch (OpNode l) lop (normalizeOpTree r)
normalizeOpTree (OpBranch (OpBranch l' lop' r') lop r)
  = normalizeOpTree (OpBranch l' lop' (OpBranch r' lop r))

fixity :: Int -> FixityDirection -> Fixity
fixity = Fixity NoSourceText

doubleWithinEps :: Double -> Double -> Double -> Bool
doubleWithinEps eps a b = abs (a - b) < eps

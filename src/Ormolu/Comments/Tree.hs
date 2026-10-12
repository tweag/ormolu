-- | The containment tree of AST element spans.
--
-- This is the structure "Ormolu.Comments.Anchor" reads to place a comment:
-- given a comment, which element encloses it most tightly, and which of
-- that element's children does it fall between. Arranging the spans by
-- containment is what makes those questions answerable from position alone,
-- without reference to the order in which the printer visits anything.
module Ormolu.Comments.Tree
  ( SpanTree (..),
    mkSpanForest,
    countNodes,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import GHC.Types.SrcLoc

-- | An element span together with the element spans it encloses. Children
-- are in ascending order and do not overlap each other.
data SpanTree = SpanTree
  { stSpan :: RealSrcSpan,
    stChildren :: [SpanTree]
  }
  deriving (Eq, Show)

-- | Arrange spans into a forest by containment.
--
-- Duplicates are dropped: several AST nodes routinely share one span (a
-- wrapper and the thing it wraps, say), and for the purpose of owning a
-- comment they are one element. Spans that overlap another without being
-- contained in it are dropped too—the GHC AST does produce such spans
-- occasionally, and they cannot be placed in a tree.
--
-- Zero-width spans are kept. An empty bracketed construct—an export or
-- import list, @[]@, a record with no fields—contains no element at all, so
-- the printer enters a zero-width one at its opening bracket
-- ('Ormolu.Printer.Combinators.locatedEmpty') to give a comment written
-- between the brackets something to attach to.
mkSpanForest :: [RealSrcSpan] -> [SpanTree]
mkSpanForest = goForest . dedupe . sortOn nestingOrder
  where
    -- Outermost first, so that a span is always seen before the spans it
    -- contains.
    nestingOrder s = (realSrcSpanStart s, Down (realSrcSpanEnd s))

    dedupe (x : y : rest) | x == y = dedupe (y : rest)
    dedupe (x : rest) = x : dedupe rest
    dedupe [] = []

    goForest [] = []
    goForest (s : rest) =
      let (children, rest') = goChildren s rest
       in SpanTree s children : goForest rest'

    goChildren parent = go []
      where
        go acc [] = (reverse acc, [])
        go acc (s : rest)
          | parent `containsSpan` s =
              let (children, rest') = goChildren s rest
               in go (SpanTree s children : acc) rest'
          | realSrcSpanStart s < realSrcSpanEnd parent =
              -- Overlaps the parent without being contained in it; there is
              -- no correct place for it, so leave it out.
              go acc rest
          | otherwise = (reverse acc, s : rest)

-- | How many elements the forest holds. Used by the tests to check that
-- duplicate and overlapping spans are dropped.
countNodes :: [SpanTree] -> Int
countNodes = sum . fmap node
  where
    node t = 1 + countNodes (stChildren t)

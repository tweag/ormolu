{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Positional comment attachment.
--
-- This module decides who owns a comment from where it sits in the source,
-- once, before anything is printed. The answer therefore does not depend on
-- the order in which the printer visits elements, which is what made
-- reordered imports and reassociated operator trees lose comments before.
--
-- The rule is short enough to state in full. Find the element that encloses
-- the comment most tightly. Within that element, find which gap between its
-- children the comment falls into. Then:
--
--   * a comment that starts on the line where the preceding sibling ends
--     trails that sibling;
--   * otherwise, if a sibling follows, the comment goes before it;
--   * otherwise the comment trails the last sibling;
--   * an element with no children at all owns the comment outright.
module Ormolu.Comments.Anchor
  ( CommentAnchor (..),
    attachComments,
    anchorFor,

    -- * Using the anchors while printing
    AnchorMap,
    mkAnchorMap,
    noComments,
    claimBefore,
    commentsBefore,
    claimTrailing,
    claimRemaining,
    pendingComments,
    commentsAnchoredWithin,
  )
where

import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import GHC.Types.SrcLoc
import Ormolu.Comments.Tree
import Ormolu.Parser.CommentStream

-- | Where a comment belongs.
data CommentAnchor
  = -- | On its own line(s) above the element
    AnchorBefore RealSrcSpan
  | -- | After the element, either on the same line or below it
    AnchorTrailing RealSrcSpan
  | -- | Inside the element, which has no children of its own
    AnchorInside RealSrcSpan
  | -- | Not inside anything: the comment belongs to the module
    AnchorModule
  deriving (Eq, Show)

-- | Attach every comment of a module.
attachComments ::
  -- | Comments, in source order
  [LComment] ->
  -- | Spans of all \"located\" elements of the module
  [RealSrcSpan] ->
  [(LComment, CommentAnchor)]
attachComments comments eltSpans =
  joinBlocks eltSpans [(c, anchorFor forest c) | c <- comments]
  where
    forest = mkSpanForest eltSpans

-- | Make a run of comment lines share one anchor.
--
-- Consecutive lines with nothing but comment between them are one block as
-- far as the reader is concerned, so splitting them across two elements
-- would tear the block apart. The first line decides where the whole block
-- goes.
joinBlocks ::
  -- | Spans of all elements, used to tell whether one stands between two
  -- comments
  [RealSrcSpan] ->
  [(LComment, CommentAnchor)] ->
  [(LComment, CommentAnchor)]
joinBlocks eltSpans = go Nothing
  where
    -- Only the start positions matter below, and only whether one of them
    -- falls in a range, so they are held as a set: this runs for every
    -- comment and scanning the module's spans each time is quadratic.
    eltStarts = Set.fromList (realSrcSpanStart <$> eltSpans)

    go _ [] = []
    go previous ((c@(L spn theComment), anchor) : rest) =
      let anchor' = case previous of
            Just (prevSpn, prevAnchor)
              | continues prevSpn -> prevAnchor
            _ -> anchor
          continues prevSpn =
            not (hasAtomsBefore theComment)
              && srcSpanEndLine prevSpn + 1 == srcSpanStartLine spn
              && not (elementBetween prevSpn spn)
       in (c, anchor') : go (Just (spn, anchor')) rest

    -- Consecutive lines are not one block if an element begins between
    -- them. @{- 0x00 -} sniExt@ followed by @{- 0x0a -} groupExt@ is two
    -- blocks, each leading its own element, not one block of two lines. It
    -- is enough for the element to *start* in the gap: in @f $ {-else-} do@
    -- the @do@ block opens on the first comment's line and runs well past
    -- the second, and the comment on the next line belongs inside it rather
    -- than to the block above.
    elementBetween from to =
      case Set.lookupGE (realSrcSpanEnd from) eltStarts of
        Just s -> s <= realSrcSpanStart to
        Nothing -> False

-- | Attach a single comment to the forest of element spans.
anchorFor :: [SpanTree] -> LComment -> CommentAnchor
anchorFor forest (L comment theComment) = go Nothing Nothing forest
  where
    go enclosing outerTrailing trees =
      case find (\t -> stSpan t `containsSpan` comment) trees of
        -- Descend into the element that encloses the comment, so that the
        -- anchor is always as tight as the source allows. Carry down the
        -- code this level has already put on the comment's line: an element
        -- that opens on that line, such as the right-hand side in @f x = --
        -- c@, has nothing of its own before the comment, but the comment
        -- still trails the @x@ one level up.
        --
        -- Only an element that wraps a single thing may carry it. One with
        -- several children is a list of items, and a comment written at the
        -- head of such a list introduces the items rather than trailing
        -- what stands before the bracket. In
        --
        -- > xs ++ [ -- why?
        -- >   a, b ]
        --
        -- the comment must stay inside the brackets; carrying it up would
        -- pull it, and the block of comment lines below it, out of the list.
        Just t
          | [_] <- stChildren t -> go (Just (stSpan t)) trailingHere (stChildren t)
          | otherwise -> go (Just (stSpan t)) Nothing (stChildren t)
        Nothing -> case (precedingSibling, followingSibling) of
          (Just p, _)
            | trailsCodeOn (stSpan p) -> AnchorTrailing (innermostEndingOnLine p)
          -- A comment with an element right after it on the same line and
          -- nothing of its own before it leads that element: a run of @{-
          -- 0x00 -} sniExt@ must not be read as trailing whatever comes
          -- before and pile up in one place. This outranks the code carried
          -- down from an outer level, so that the @{-a-}@ of @x = ({-a-} b,
          -- c)@ stays with @b@ rather than being pulled out to trail the
          -- @x@.
          (_, Just n)
            | startsOnCommentLine (stSpan n) -> AnchorBefore (stSpan n)
          -- Nothing at this level stands before the comment, but an outer
          -- level put code on its line: the comment trails that code. This
          -- is what keeps @f x = -- c@ on one line, the right-hand side
          -- having opened on that line with the comment as its first
          -- content.
          --
          -- Only a line comment may do this. It runs to the end of the line
          -- either way, so trailing an element one level up still renders
          -- it exactly where it was written. A block comment renders in
          -- place instead, and would end up ahead of the tokens that opened
          -- the element it was written inside: the pragma of @corebar = {-#
          -- CORE "bar baz" #-}@ would move before the @=@.
          (Nothing, _)
            | not (isMultilineComment theComment),
              Just p <- outerTrailing ->
                AnchorTrailing (innermostEndingOnLine p)
          (_, Just n) -> AnchorBefore (stSpan n)
          -- A comment after the last child of an element belongs to that
          -- element, but a comment after everything at the top level
          -- belongs to the module: there is nothing it can trail without
          -- being rendered before syntax that preceded it in the input,
          -- such as the @where@ of a module header.
          (Just p, Nothing)
            | Just _ <- enclosing -> AnchorTrailing (stSpan p)
            | otherwise -> AnchorModule
          (Nothing, Nothing) -> maybe AnchorModule AnchorInside enclosing
          where
            followingSibling =
              listToMaybe
                [ t
                | t <- trees,
                  realSrcSpanStart (stSpan t) >= realSrcSpanEnd comment
                ]
            startsOnCommentLine s =
              srcSpanStartLine s == srcSpanEndLine comment
      where
        precedingSibling =
          lastMaybe
            [ t
            | t <- trees,
              realSrcSpanEnd (stSpan t) <= realSrcSpanStart comment
            ]
        trailingHere = case precedingSibling of
          Just p | trailsCodeOn (stSpan p) -> Just p
          _ -> outerTrailing

    -- A comment only trails an element when it really does sit after code
    -- on that line. Checking the line alone is not enough, because the AST
    -- has zero-width spans that happen to share a line with a comment while
    -- standing before it.
    trailsCodeOn s =
      srcSpanEndLine s == srcSpanStartLine comment
        && hasAtomsBefore theComment

    -- A comment that trails a bracketed construct belongs to the innermost
    -- element that ends on its line, not to the bracket: @(x + y) -- c@
    -- attaches to @y@, so that the comment is rendered next to the
    -- expression it was written next to rather than after the closing
    -- bracket.
    innermostEndingOnLine t =
      case lastMaybe (filter (trailsCodeOn . stSpan) (stChildren t)) of
        Nothing -> stSpan t
        Just t' -> innermostEndingOnLine t'

    lastMaybe xs = if null xs then Nothing else Just (last xs)

----------------------------------------------------------------------------
-- Using the anchors while printing

-- | Anchored comments, arranged so that the printer can look them up by the
-- span of the element it is entering or leaving.
--
-- Comments are claimed rather than consumed: the first element with a given
-- span takes them, and every later element with the same span finds
-- nothing. Since several AST nodes routinely share a span, and the printer
-- enters them outermost first, this gives the comment to the outermost of
-- them, which is what one wants—a comment belongs outside the parentheses,
-- not inside them.
data AnchorMap = AnchorMap
  { amBefore :: Map RealSrcSpan [LComment],
    amTrailing :: Map RealSrcSpan [LComment],
    amModule :: [LComment]
  }

-- | An empty map, for the first of the two rendering passes: it collects
-- the spans of the elements the printer enters, and emits no comments.
noComments :: AnchorMap
noComments =
  AnchorMap {amBefore = Map.empty, amTrailing = Map.empty, amModule = []}

-- | Arrange anchored comments for lookup.
--
-- __NOTE__: 'AnchorInside' is currently folded into 'AnchorTrailing'. Doing
-- it properly needs a combinator for elements that can have no children at
-- all.
mkAnchorMap :: [(LComment, CommentAnchor)] -> AnchorMap
mkAnchorMap anchored =
  AnchorMap
    { amBefore = collect [(spn, c) | (c, AnchorBefore spn) <- anchored],
      amTrailing =
        collect $
          [(spn, c) | (c, AnchorTrailing spn) <- anchored]
            <> [(spn, c) | (c, AnchorInside spn) <- anchored],
      amModule = [c | (c, AnchorModule) <- anchored]
    }
  where
    collect = Map.fromListWith (flip (<>)) . fmap (fmap pure)

-- | The comments that go before the element with the given span, without
-- claiming them.
commentsBefore :: RealSrcSpan -> AnchorMap -> [LComment]
commentsBefore spn = Map.findWithDefault [] spn . amBefore

-- | Claim the comments that go before the element with the given span.
claimBefore :: RealSrcSpan -> AnchorMap -> ([LComment], AnchorMap)
claimBefore spn am =
  case Map.lookup spn (amBefore am) of
    Nothing -> ([], am)
    Just cs -> (cs, am {amBefore = Map.delete spn (amBefore am)})

-- | Claim the comments that go after the element with the given span.
claimTrailing :: RealSrcSpan -> AnchorMap -> ([LComment], AnchorMap)
claimTrailing spn am =
  case Map.lookup spn (amTrailing am) of
    Nothing -> ([], am)
    Just cs -> (cs, am {amTrailing = Map.delete spn (amTrailing am)})

-- | Claim everything that is left: the comments that belong to no element,
-- plus anything that was anchored to an element the printer never entered.
claimRemaining :: AnchorMap -> ([LComment], AnchorMap)
claimRemaining am =
  ( pendingComments am,
    AnchorMap {amBefore = Map.empty, amTrailing = Map.empty, amModule = []}
  )

-- | The comments anchored to the element at the given span, or to any
-- element inside it.
--
-- This is the question the layout decision needs to ask. "Which comments
-- are contained in this element" is a different and much coarser one: a
-- comment anywhere in a declaration is contained in it, but is attached to
-- one particular element, and only that element's layout should have to
-- account for it. This runs for every element the printer enters, so it
-- must not walk the whole map. Anchors are 'RealSrcSpan's ordered by start
-- position, and all of a module's spans share a file, so the anchors that
-- could be contained in the region are the contiguous run whose start lies
-- within it. Cutting the map down to that run first makes the cost
-- proportional to the size of the region rather than to the number of
-- comments in the module.
commentsAnchoredWithin :: RealSrcSpan -> AnchorMap -> [LComment]
commentsAnchoredWithin region AnchorMap {..} =
  sortOn getLoc . concat $
    within amBefore <> within amTrailing
  where
    within =
      Map.elems
        . Map.filterWithKey (\anchor _ -> region `containsSpan` anchor)
        . startingWithin

    -- Antitone in map order: as keys ascend their start position never
    -- decreases, so each predicate holds on a prefix and then stops.
    startingWithin =
      fst
        . Map.spanAntitone ((<= realSrcSpanEnd region) . realSrcSpanStart)
        . snd
        . Map.spanAntitone ((< realSrcSpanStart region) . realSrcSpanStart)

-- | Every comment that has not been emitted yet, in source order.
pendingComments :: AnchorMap -> [LComment]
pendingComments AnchorMap {..} =
  sortOn getLoc $
    amModule
      <> concat (Map.elems amBefore)
      <> concat (Map.elems amTrailing)

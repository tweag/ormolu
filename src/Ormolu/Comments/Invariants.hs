{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Properties that comment handling has to satisfy, and the check that
-- enforces them.
--
-- Every comment of the input should come out exactly once, and in the order
-- it went in. The check runs on every run of Ormolu, alongside the check
-- that the AST is unchanged, and is disabled by the same @--unsafe@ flag.
--
-- This is the half of comment checking that works on where the comments went
-- rather than on what they say. It compares the /spans/ of the comments a
-- module started with against the spans recorded as the printer emitted
-- them, so it can name the comment that was dropped, duplicated, invented
-- or moved.
--
-- It does /not/ look at the text of a comment at all: rendering one with
-- its contents mangled would pass. That is the other half, and it belongs
-- to 'Ormolu.Diff.ParseResult.diffCommentStream', which compares text and
-- ignores position. Neither check subsumes the other and both run by
-- default.
--
-- Haddocks are outside both halves. GHC's parser makes them part of the AST
-- rather than leaving them in the comment stream, so they are neither among
-- the comments a module started with nor in what the text check compares.
-- Losing or duplicating one changes the AST itself, and that is caught by
-- the third check, 'Ormolu.Diff.ParseResult.diffParseResult' comparing the
-- two syntax trees.
module Ormolu.Comments.Invariants
  ( InvariantViolation (..),
    checkCommentInvariants,
    renderInvariantViolation,
  )
where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Types.SrcLoc
import Ormolu.Printer.CommentPlacement

-- | A way in which the emitted comments failed to correspond to the
-- comments of the input.
data InvariantViolation
  = -- | A comment of the input was never emitted
    CommentDropped RealSrcSpan
  | -- | A comment was emitted more than once, the given number of times
    CommentDuplicated RealSrcSpan Int
  | -- | A comment was emitted that does not correspond to any comment of
    -- the input
    CommentInvented RealSrcSpan
  | -- | A comment was emitted after one that comes later in the input. The
    -- first span is the comment that was emitted too late, the second is
    -- the one it should have preceded.
    CommentReordered RealSrcSpan RealSrcSpan
  deriving (Eq, Show)

-- | Compare the comments of a snippet against the comments that were
-- emitted while rendering it.
checkCommentInvariants ::
  -- | Spans of all the comments the snippet started with
  [RealSrcSpan] ->
  -- | Spans of the elements the formatter is allowed to reorder, so that
  -- the comments travelling with them are exempt from the order check
  [RealSrcSpan] ->
  -- | Placements recorded while rendering it, in the order of emission
  [CommentPlacement] ->
  [InvariantViolation]
checkCommentInvariants inputSpans reorderable placements =
  dropped <> duplicated <> invented <> reordered
  where
    emitted = cpSpan <$> placements
    -- Pragmas and imports are deliberately sorted and the comments attached
    -- to them travel along, so the order they come out in says nothing.
    -- They are still expected to come out exactly once, which is what
    -- catches a comment being duplicated.
    ordered =
      [ spn
      | CommentPlacement {cpSpan = spn, cpSlot} <- placements,
        cpSlot /= SlotPragma,
        not (travelsWithAReorderedElement cpSlot)
      ]
    travelsWithAReorderedElement slot = case slotAnchor slot of
      Nothing -> False
      Just anchor -> any (`containsSpan` anchor) reorderable
    inputSet = Map.fromList ((,()) <$> inputSpans)
    counts = Map.fromListWith (+) ((,1 :: Int) <$> emitted)

    dropped =
      [CommentDropped spn | spn <- sort inputSpans, not (spn `Map.member` counts)]
    duplicated =
      [ CommentDuplicated spn n
      | (spn, n) <- Map.toAscList counts,
        n > 1
      ]
    invented =
      [ CommentInvented spn
      | spn <- Map.keys counts,
        not (spn `Map.member` inputSet)
      ]

    -- Only the first emission of each comment is considered, so that a
    -- comment reported as duplicated is not also reported as reordered.
    reordered = go [] (dedupe [] ordered)
      where
        dedupe _ [] = []
        dedupe seen (x : xs)
          | x `elem` seen = dedupe seen xs
          | otherwise = x : dedupe (x : seen) xs
        go _ [] = []
        go seen (x : xs) =
          [CommentReordered x y | y <- seen, x < y]
            <> go (x : seen) xs

-- | Render a violation as a single line.
renderInvariantViolation :: InvariantViolation -> Text
renderInvariantViolation = \case
  CommentDropped spn ->
    "dropped     " <> renderSpan spn
  CommentDuplicated spn n ->
    "duplicated  " <> renderSpan spn <> " (emitted " <> showT n <> " times)"
  CommentInvented spn ->
    "invented    " <> renderSpan spn
  CommentReordered spn before ->
    "reordered   " <> renderSpan spn <> " (emitted after " <> renderSpan before <> ")"

renderSpan :: RealSrcSpan -> Text
renderSpan spn =
  renderLoc (realSrcSpanStart spn) <> "-" <> renderLoc (realSrcSpanEnd spn)
  where
    renderLoc l = showT (srcLocLine l) <> ":" <> showT (srcLocCol l)

showT :: (Show a) => a -> Text
showT = T.pack . show

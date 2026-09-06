{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A record of where each comment ended up in the rendered output.
--
-- The printer notes every comment as it emits it. Ormolu then checks that
-- record against the comments of the input, which is how it can promise
-- that formatting neither drops, duplicates, invents nor reorders a
-- comment; see "Ormolu.Comments.Invariants".
module Ormolu.Printer.CommentPlacement
  ( CommentPlacement (..),
    CommentSlot (..),
    slotAnchor,
  )
where

import GHC.Types.SrcLoc

----------------------------------------------------------------------------
-- Types

-- | Where a comment ended up relative to the AST element it was attached
-- to.
--
-- Only the distinctions a consumer can act on are kept: whether the comment
-- was attached to an element, and whether it rode along with a pragma. See
-- "Ormolu.Comments.Invariants", which is what reads this.
data CommentSlot
  = -- | Attached to the element at this span
    SlotAt RealSrcSpan
  | -- | Hoisted into the module header along with a pragma. Pragmas are
    -- sorted on purpose, so the order such a comment comes out in says
    -- nothing.
    SlotPragma
  | -- | Attached to nothing: the Stack header, or a leftover flushed at the
    -- end of the module by 'Ormolu.Printer.Comments.spitRemainingComments'
    SlotFloating
  deriving (Eq, Show)

-- | The span of the AST element that a comment was attached to, if the
-- comment was attached to an element at all.
slotAnchor :: CommentSlot -> Maybe RealSrcSpan
slotAnchor = \case
  SlotAt spn -> Just spn
  SlotPragma -> Nothing
  SlotFloating -> Nothing

-- | A single placement decision: one comment and the slot it was rendered
-- in.
data CommentPlacement = CommentPlacement
  { -- | Span of the comment in the input, which is what identifies it
    cpSpan :: RealSrcSpan,
    -- | Where the comment ended up
    cpSlot :: CommentSlot
  }
  deriving (Eq, Show)

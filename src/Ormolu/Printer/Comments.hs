{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for formatting comments. This is low-level code; use
-- "Ormolu.Printer.Combinators" unless you know what you are doing.
module Ormolu.Printer.Comments
  ( spitPrecedingComments,
    spitFollowingComments,
    spitRemainingComments,
    spitCommentNow,
    spitCommentPending,
    CommentSlot (..),
  )
where

import Control.Monad
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import GHC.Types.SrcLoc
import Ormolu.Comments.Anchor
import Ormolu.Parser.CommentStream
import Ormolu.Printer.CommentPlacement
import Ormolu.Printer.Internal

----------------------------------------------------------------------------
-- Top-level

-- | Output all preceding comments for an element at the given location.
spitPrecedingComments ::
  -- | Span of the element to attach comments to
  RealSrcSpan ->
  R ()
spitPrecedingComments ref = do
  comments <- withAnchorMap (claimBefore ref)
  forM_ comments (spitPrecedingComment ref)
  unless (null comments) $ do
    lastEmitted <- getLastEmitted
    -- Insert a blank line between the preceding comments and the thing
    -- after them if there was a blank line in the input.
    when (needsNewlineBefore ref lastEmitted) newline

-- | Output all comments following an element at the given location.
spitFollowingComments ::
  -- | Span of the element to attach comments to
  RealSrcSpan ->
  R ()
spitFollowingComments ref = do
  comments <- withAnchorMap (claimTrailing ref)
  forM_ comments (spitFollowingComment ref)

-- | Output every comment that no element claimed.
--
-- This is the safety net that keeps a misattached comment from being lost
-- outright. It also means misattachment is silent, which is why
-- "Ormolu.Comments.Invariants" exists.
spitRemainingComments :: R ()
spitRemainingComments = do
  -- Make sure we have a blank line between the last definition and the
  -- trailing comments.
  newline
  comments <- withAnchorMap claimRemaining
  forM_ comments spitRemainingComment

----------------------------------------------------------------------------
-- Single-comment functions

-- | Output a single preceding comment for an element at the given location.
spitPrecedingComment ::
  -- | Span of the element the comment is attached to
  RealSrcSpan ->
  -- | The comment to output
  LComment ->
  R ()
spitPrecedingComment ref (L l comment) = do
  lastEmitted <- getLastEmitted
  lineSpans <- thisLineSpans
  let thisCommentLine = srcLocLine (realSrcSpanStart l)
      needsNewline =
        case listToMaybe lineSpans of
          Nothing -> False
          Just spn -> srcLocLine (realSrcSpanEnd spn) /= thisCommentLine
      sameLine = theSameLinePre l ref
  when (needsNewline || needsNewlineBefore l lastEmitted) newline
  spitCommentNow (SlotAt ref) l comment
  if sameLine
    then space
    else newline

-- | Output a single comment that follows an element at the given location.
spitFollowingComment ::
  -- | Span of the element the comment is attached to
  RealSrcSpan ->
  -- | The comment to output
  LComment ->
  R ()
spitFollowingComment ref (L l comment) = do
  lastEmitted <- getLastEmitted
  if theSameLinePost l ref
    then
      if isMultilineComment comment
        then space >> spitCommentNow (SlotAt ref) l comment
        else spitCommentPending (SlotAt ref) OnTheSameLine l comment
    else do
      -- A comment keeps the blank line the input had in front of it. When
      -- nothing carrying a position has been emitted since, the element the
      -- comment is attached to is what that blank line separated it from.
      let lastEmitted' = case lastEmittedSpan lastEmitted of
            Just _ -> lastEmitted
            Nothing -> LastEmittedComment ref
      when (needsNewlineBefore l lastEmitted') $
        registerPendingCommentLine OnNextLine ""
      spitCommentPending (SlotAt ref) OnNextLine l comment

-- | Output a single unclaimed comment.
spitRemainingComment ::
  -- | The comment to output
  LComment ->
  R ()
spitRemainingComment (L l comment) = do
  lastEmitted <- getLastEmitted
  when (needsNewlineBefore l lastEmitted) newline
  spitCommentNow SlotFloating l comment
  newline

----------------------------------------------------------------------------
-- Helpers

-- | Determine whether we need to insert a newline between the current
-- comment and the last printed comment.
needsNewlineBefore ::
  -- | Current comment span
  RealSrcSpan ->
  -- | What was emitted last
  LastEmitted ->
  Bool
needsNewlineBefore _ (LastEmittedHaddock _) = True
needsNewlineBefore l lastEmitted =
  case lastEmittedSpan lastEmitted of
    Nothing -> False
    Just lastSpn ->
      srcSpanStartLine l > srcSpanEndLine lastSpn + 1

-- | Are the preceding comment and the AST element on the same line?
theSameLinePre ::
  -- | Current comment span
  RealSrcSpan ->
  -- | AST element location
  RealSrcSpan ->
  Bool
theSameLinePre l ref =
  srcSpanEndLine l == srcSpanStartLine ref

-- | Are the following comment and the AST element on the same line?
theSameLinePost ::
  -- | Current comment span
  RealSrcSpan ->
  -- | AST element location
  RealSrcSpan ->
  Bool
theSameLinePost l ref =
  srcSpanStartLine l == srcSpanEndLine ref

-- | Output a 'Comment' immediately. This is a low-level printing function.
--
-- Note that it records the placement as well as printing. Every path that
-- emits a comment has to go through this or 'spitCommentPending', or
-- "Ormolu.Comments.Invariants" will report the comment as dropped and
-- Ormolu will refuse to format the file.
spitCommentNow ::
  -- | The slot the comment is being rendered in
  CommentSlot ->
  RealSrcSpan ->
  Comment ->
  R ()
spitCommentNow slot spn comment = do
  recordCommentPlacement CommentPlacement {cpSpan = spn, cpSlot = slot}
  sitcc
    . sequence_
    . NE.intersperse newline
    . fmap txt
    . unComment
    $ comment
  setLastEmitted (LastEmittedComment spn)

-- | Output a 'Comment' at the end of the correct line, or after it,
-- depending on the 'CommentPosition'. Used for comments that may follow on
-- the same line as something we just rendered, but not immediately after it.
spitCommentPending ::
  -- | The slot the comment is being rendered in
  CommentSlot ->
  CommentPosition ->
  RealSrcSpan ->
  Comment ->
  R ()
spitCommentPending slot position spn comment = do
  recordCommentPlacement CommentPlacement {cpSpan = spn, cpSlot = slot}
  let wrapper = case position of
        OnTheSameLine -> sitcc
        OnNextLine -> id
  wrapper
    . sequence_
    . NE.toList
    . fmap (registerPendingCommentLine position)
    . unComment
    $ comment
  setLastEmitted (LastEmittedComment spn)

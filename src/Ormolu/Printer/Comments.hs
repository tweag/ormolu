{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for formatting of comments. This is low-level code, use
-- "Ormolu.Printer.Combinators" unless you know what you are doing.

module Ormolu.Printer.Comments
  ( spitPrecedingComments
  , spitFollowingComments
  , spitRemainingComments
  )
where

import Control.Monad
import Data.Coerce (coerce)
import Data.Data (Data)
import Ormolu.Parser.CommentStream
import Ormolu.Printer.Internal
import Ormolu.Utils (isModule)
import SrcLoc
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

----------------------------------------------------------------------------
-- Top-level

-- | Output all preceding comments for an element at given location.

spitPrecedingComments
  :: Data a
  => RealLocated a              -- ^ AST element to attach comments to
  -> R ()
spitPrecedingComments = handleCommentSeries . spitPrecedingComment

-- | Output all comments following an element at given location.

spitFollowingComments
  :: Data a
  => RealLocated a              -- ^ AST element of attach comments to
  -> R ()
spitFollowingComments ref = do
  trimSpanStream (getLoc ref)
  handleCommentSeries (spitFollowingComment ref)

-- | Output all remaining comments in the comment stream.

spitRemainingComments :: R ()
spitRemainingComments = handleCommentSeries spitRemainingComment

----------------------------------------------------------------------------
-- Single-comment functions

-- | Output a single preceding comment for an element at given location.

spitPrecedingComment
  :: Data a
  => RealLocated a              -- ^ AST element to attach comments to
  -> Maybe RealSrcSpan          -- ^ Location of last comment in the series
  -> R Bool                     -- ^ Are we done?
spitPrecedingComment (L ref a) mlastSpn = do
  let p (L l _) = realSrcSpanEnd l <= realSrcSpanStart ref
  withPoppedComment p $ \l comment -> do
    dirtyLine <-
      case mlastSpn of
        -- NOTE When the current line is dirty it means that something that
        -- can have comments attached to it is already on the line. To avoid
        -- problems with idempotence we cannot output the first comment
        -- immediately because it'll be attached to the previous element (on
        -- the same line) on the next run, so we play safe here and output
        -- an extra 'newline' in this case.
        Nothing -> isLineDirty -- only for very first preceding comment
        Just _ -> return False
    when (dirtyLine || needsNewlineBefore l mlastSpn) newline
    spitCommentNow l comment
    if theSameLinePre l ref && not (isModule a)
      then space
      else newline

-- | Output a comment that follows element at given location immediately on
-- the same line, if there is any.

spitFollowingComment
  :: Data a
  => RealLocated a              -- ^ AST element to attach comments to
  -> Maybe RealSrcSpan          -- ^ Location of last comment in the series
  -> R Bool                     -- ^ Are we done?
spitFollowingComment (L ref a) mlastSpn = do
  mnSpn <- nextEltSpan
  -- Get first enclosing span that is not equal to reference span, i.e. it's
  -- truly something enclosing the AST element.
  meSpn <- getEnclosingSpan (/= ref)
  withPoppedComment (commentFollowsElt ref mnSpn meSpn mlastSpn) $ \l comment ->
    if theSameLinePost l ref && not (isModule a)
      then spitCommentPending OnTheSameLine l comment
      else do
        when (needsNewlineBefore l mlastSpn) $
          registerPendingCommentLine OnNextLine ""
        spitCommentPending OnNextLine l comment

-- | Output a single remaining comment from the comment stream.

spitRemainingComment
  :: Maybe RealSrcSpan          -- ^ Location of last comment in the series
  -> R Bool                     -- ^ Are we done?
spitRemainingComment mlastSpn =
  withPoppedComment (const True) $ \l comment -> do
    when (needsNewlineBefore l mlastSpn) newline
    spitCommentNow l comment
    newline

----------------------------------------------------------------------------
-- Helpers

-- | Output series of comments.

handleCommentSeries
  :: (Maybe RealSrcSpan -> R Bool)
     -- ^ Given location of previous comment, output the next comment
     -- returning 'True' if we're done
  -> R ()
handleCommentSeries f = go
  where
    go = do
      done <- getLastCommentSpan >>= f
      unless done go

-- | Try to pop a comment using given predicate and if there is a comment
-- matching the predicate, print it out.

withPoppedComment
  :: (RealLocated Comment -> Bool) -- ^ Comment predicate
  -> (RealSrcSpan -> Comment -> R ()) -- ^ Printing function
  -> R Bool                        -- ^ Are we done?
withPoppedComment p f = do
  r <- popComment p
  case r of
    Nothing -> return True
    Just (L l comment) -> False <$ f l comment

-- | Determine if we need to insert a newline between current comment and
-- last printed comment.

needsNewlineBefore
  :: RealSrcSpan                -- ^ Current comment span
  -> Maybe RealSrcSpan          -- ^ Last printed comment span
  -> Bool
needsNewlineBefore l mlastSpn =
  case mlastSpn of
    Nothing -> False
    Just lastSpn ->
      srcSpanStartLine l > srcSpanEndLine lastSpn + 1

-- | Is the preceding comment and AST element are on the same line?

theSameLinePre
  :: RealSrcSpan                -- ^ Current comment span
  -> RealSrcSpan                -- ^ AST element location
  -> Bool
theSameLinePre l ref =
  srcSpanEndLine l == srcSpanStartLine ref

-- | Is the following comment and AST element are on the same line?

theSameLinePost
  :: RealSrcSpan                -- ^ Current comment span
  -> RealSrcSpan                -- ^ AST element location
  -> Bool
theSameLinePost l ref =
  srcSpanStartLine l == srcSpanEndLine ref

-- | Determine if given comment follows AST element.

commentFollowsElt
  :: RealSrcSpan                -- ^ Location of AST element
  -> Maybe RealSrcSpan          -- ^ Location of next AST element
  -> Maybe RealSrcSpan          -- ^ Location of enclosing AST element
  -> Maybe RealSrcSpan          -- ^ Location of last comment in the series
  -> RealLocated Comment        -- ^ Comment to test
  -> Bool
commentFollowsElt ref mnSpn meSpn mlastSpn (L l comment) =
  -- A comment follows a AST element if all 4 conditions are satisfied:
  goesAfter
    && logicallyFollows
    && noEltBetween
    && (continuation || lastInEnclosing || supersedesParentElt)
  where
    -- 1) The comment starts after end of the AST element:
    goesAfter =
      realSrcSpanStart l >= realSrcSpanEnd ref
    -- 2) The comment logically belongs to the element, four cases:
    logicallyFollows
      = theSameLinePost l ref -- a) it's on the same line
      || isPrevHaddock comment -- b) it's a Haddock string starting with -- ^
      || continuation -- c) it's a continuation of a comment block
      || lastInEnclosing -- d) it's the last element in the enclosing construct

    -- 3) There is no other AST element between this element and the comment:
    noEltBetween =
      case mnSpn of
        Nothing -> True
        Just nspn ->
          realSrcSpanStart nspn >= realSrcSpanEnd l
    -- 4) Less obvious: if column of comment is closer to the start of
    -- enclosing element, it probably related to that parent element, not to
    -- the current child element. This rule is important because otherwise
    -- all comments would end up assigned to closest inner elements, and
    -- parent elements won't have a chance to get any comments assigned to
    -- them. This is not OK because comments will get indented according to
    -- the AST elements they are attached to.
    --
    -- Skip this rule if the comment is a continuation of a comment block.
    supersedesParentElt =
      case meSpn of
        Nothing -> True
        Just espn ->
          let startColumn = srcLocCol . realSrcSpanStart
          in if startColumn espn > startColumn ref
               then True
               else abs (startColumn espn - startColumn l)
                      >= abs (startColumn ref - startColumn l)
    continuation =
      case mlastSpn of
        Nothing -> False
        Just spn -> srcSpanEndLine spn + 1 == srcSpanStartLine l

    lastInEnclosing =
      case meSpn of
        -- When there is no enclosing element, return false
        Nothing -> False
        -- When there is an enclosing element,
        Just espn ->
          let -- Make sure that the comment is inside the enclosing element
              insideParent = realSrcSpanEnd l <= realSrcSpanEnd espn
              -- And check if the next element is outside of the parent
              nextOutsideParent = case mnSpn of
                Nothing -> True
                Just nspn -> realSrcSpanEnd espn < realSrcSpanStart nspn
           in insideParent && nextOutsideParent

-- | Output a 'Comment' immediately. This is a low-level printing function.

spitCommentNow :: RealSrcSpan -> Comment -> R ()
spitCommentNow spn comment = do
  sitcc
    . sequence_
    . NE.intersperse newline
    . fmap (txt . T.pack)
    . coerce
    $ comment
  setLastCommentSpan spn

-- | Output a 'Comment' at the end of correct line or after it depending on
-- 'CommentPosition'. Used for comments that may potentially follow on the
-- same line as something we just rendered, but not immediately after it.

spitCommentPending :: CommentPosition -> RealSrcSpan -> Comment -> R ()
spitCommentPending position spn comment = do
  let wrapper = case position of
        OnTheSameLine -> sitcc
        OnNextLine -> id
  wrapper
    . sequence_
    . NE.toList
    . fmap (registerPendingCommentLine position . T.pack)
    . coerce
    $ comment
  setLastCommentSpan spn

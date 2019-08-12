{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for formatting of comments. This is low-level code, use
-- "Ormolu.Printer.Combinators" unless you know what you are doing.

module Ormolu.Printer.Comments
  ( spitPrecedingComments
  , spitFollowingComments
  , spitRemainingComments
  , hasMoreComments
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
  -> R (Maybe RealSrcSpan)      -- ^ Location of this comment
spitPrecedingComment (L ref a) mlastSpn = do
  let p (L l _) = realSrcSpanEnd l <= realSrcSpanStart ref
  withPoppedComment p $ \l comment -> do
    when (needsNewlineBefore l mlastSpn) newline
    spitComment comment
    if theSameLine l ref && not (isModule a)
      then spit " "
      else newline

-- | Output a comment that follows element at given location immediately on
-- the same line, if there is any.

spitFollowingComment
  :: Data a
  => RealLocated a              -- ^ AST element to attach comments to
  -> Maybe RealSrcSpan          -- ^ Location of last comment in the series
  -> R (Maybe RealSrcSpan)      -- ^ Location of this comment
spitFollowingComment (L ref a) mlastSpn = do
  mnSpn <- nextEltSpan
  meSpn <- getEnclosingSpan ref
  newlineModified <- isNewlineModified
  i <- getIndent
  withPoppedComment (commentFollowsElt ref mnSpn meSpn mlastSpn) $ \l comment ->
    if theSameLine l ref && not (isModule a)
      then modNewline $ \m -> setIndent i $ do
        if newlineModified
          then do
            -- This happens when we have several lines each with its own
            -- comment and they get merged by the formatter.
            m
            spitComment comment
            newline
          else do
            spit " "
            spitComment comment
            m
      else modNewline $ \m -> setIndent i $ do
        m
        when (needsNewlineBefore l mlastSpn) newline
        spitComment comment
        newline

-- | Output a single remaining comment from the comment stream.

spitRemainingComment
  :: Maybe RealSrcSpan          -- ^ Location of last comment in the series
  -> R (Maybe RealSrcSpan)      -- ^ Location of this comment
spitRemainingComment mlastSpn =
  withPoppedComment (const True) $ \l comment -> do
    when (needsNewlineBefore l mlastSpn) newline
    spitComment comment
    newline

----------------------------------------------------------------------------
-- Helpers

-- | Output series of comments.

handleCommentSeries
  :: (Maybe RealSrcSpan -> R (Maybe RealSrcSpan))
     -- ^ Given location of previous comment, output the next comment
     -- returning its location, or 'Nothing' if we are done
  -> R ()
handleCommentSeries f = go Nothing
  where
    go mlastSpn = do
      r <- f mlastSpn
      case r of
        Nothing -> return ()
        Just spn -> go (Just spn)

-- | Try to pop a comment using given predicate and if there is a comment
-- matching the predicate, print it out.

withPoppedComment
  :: (RealLocated Comment -> Bool) -- ^ Comment predicate
  -> (RealSrcSpan -> Comment -> R ()) -- ^ Priting function
  -> R (Maybe RealSrcSpan)
withPoppedComment p f = do
  r <- popComment p
  case r of
    Nothing -> return Nothing
    Just (L l comment) -> Just l <$ f l comment

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

-- | Is the comment and AST element are on the same line?

theSameLine
  :: RealSrcSpan                -- ^ Current comment span
  -> RealSrcSpan                -- ^ AST element location
  -> Bool
theSameLine l ref =
  srcSpanEndLine l == srcSpanStartLine ref

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
    && (continuation || supersedesParentElt)
  where
    -- 1) The comment starts after end of the AST element:
    goesAfter =
      realSrcSpanStart l >= realSrcSpanEnd ref
    -- 2) The comment logically belongs to the element, three cases:
    logicallyFollows
      = theSameLine l ref -- a) it's on the same line
      || isPrevHaddock comment -- b) it's a Haddock string starting with -- ^
      || continuation -- c) it's a continuation of a comment block
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

-- | Output a 'Comment'. This is a low-level printing function.

spitComment :: Comment -> R ()
spitComment =
  sequence_ . NE.intersperse newline . fmap f . coerce
  where
    f x = ensureIndent >> spit (T.pack x)

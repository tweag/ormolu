{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

-- | In most cases import "Ormolu.Printer.Combinators" instead, these
-- functions are the low-level building blocks and should not be used on
-- their own. The 'R' monad is re-exported from "Ormolu.Printer.Combinators"
-- as well.

module Ormolu.Printer.Internal
  ( -- * The 'R' monad
    R
  , runR
    -- * Internal functions
  , txt
  , atom
  , space
  , newline
  , isLineDirty
  , inci
  , sitcc
  , Layout (..)
  , enterLayout
  , vlayout
    -- * Helpers for braces
  , useBraces
  , dontUseBraces
  , canUseBraces
    -- * Special helpers for comment placement
  , CommentPosition (..)
  , registerPendingCommentLine
  , trimSpanStream
  , nextEltSpan
  , popComment
  , getEnclosingSpan
  , withEnclosingSpan
  , setLastCommentSpan
  , getLastCommentSpan
    -- * Annotations
  , getAnns
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bool (bool)
import Data.Coerce
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Lazy.Builder
import GHC
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Printer.SpanStream
import Ormolu.Utils (showOutputable)
import Outputable (Outputable)
import SrcLoc
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

----------------------------------------------------------------------------
-- The 'R' monad

-- | The 'R' monad hosts combinators that allow us to describe how to render
-- AST.

newtype R a = R (ReaderT RC (State SC) a)
  deriving (Functor, Applicative, Monad)

-- | Reader context of 'R'. This should be used when we control rendering by
-- enclosing certain expressions with wrappers.

data RC = RC
  { rcIndent :: !Int
    -- ^ Indentation level, as the column index we need to start from after
    -- a newline if we break lines
  , rcLayout :: Layout
    -- ^ Current layout
  , rcEnclosingSpans :: [RealSrcSpan]
    -- ^ Spans of enclosing elements of AST
  , rcAnns :: Anns
    -- ^ Collection of annotations
  , rcCanUseBraces :: Bool
    -- ^ If the last expression in the layout can use braces
  }

-- | State context of 'R'.

data SC = SC
  { scColumn :: !Int
    -- ^ Index of the next column to render
  , scBuilder :: Builder
    -- ^ Rendered source code so far
  , scSpanStream :: SpanStream
    -- ^ Span stream
  , scCommentStream :: CommentStream
    -- ^ Comment stream
  , scPendingComments :: ![(CommentPosition, Int, Text)]
    -- ^ Pending comment lines (in reverse order) to be inserted before next
    -- newline, 'Int' is the indentation level
  , scDirtyLine :: !Bool
    -- ^ Whether the current line is “dirty”, that is, already contains
    -- atoms that can have comments attached to them
  , scRequestedDelimiter :: !RequestedDelimiter
    -- ^ Whether to output a space before the next output
  , scLastCommentSpan :: !(Maybe RealSrcSpan)
    -- ^ Span of last output comment
  }

-- | Make sure next output is delimited by one of the following.

data RequestedDelimiter
  = RequestedSpace              -- ^ A space
  | RequestedNewline            -- ^ A newline
  | RequestedNothing            -- ^ Nothing
  | AfterNewline                -- ^ We just output a newline
  | VeryBeginning               -- ^ We haven't printed anything yet
  deriving (Eq, Show)

-- | 'Layout' options.

data Layout
  = SingleLine                  -- ^ Put everything on single line
  | MultiLine                   -- ^ Use multiple lines
  deriving (Eq, Show)

-- | Modes for rendering of pending comments.

data CommentPosition
  = OnTheSameLine               -- ^ Put the comment on the same line
  | OnNextLine                  -- ^ Put the comment on next line
  deriving (Eq, Show)

-- | Run an 'R' monad.

runR
  :: R ()                       -- ^ Monad to run
  -> SpanStream                 -- ^ Span stream
  -> CommentStream              -- ^ Comment stream
  -> Anns                       -- ^ Annotations
  -> Text                       -- ^ Resulting rendition
runR (R m) sstream cstream anns =
  TL.toStrict . toLazyText . scBuilder $ execState (runReaderT m rc) sc
  where
    rc = RC
      { rcIndent = 0
      , rcLayout = MultiLine
      , rcEnclosingSpans = []
      , rcAnns = anns
      , rcCanUseBraces = False
      }
    sc = SC
      { scColumn = 0
      , scBuilder = mempty
      , scSpanStream = sstream
      , scCommentStream = cstream
      , scPendingComments = []
      , scDirtyLine = False
      , scRequestedDelimiter = VeryBeginning
      , scLastCommentSpan = Nothing
      }

----------------------------------------------------------------------------
-- Internal functions

-- | Output a fixed 'Text' fragment. The argument may not contain any line
-- breaks. 'txt' is used to output all sorts of “fixed” bits of syntax like
-- keywords and pipes @|@ in functional dependencies.
--
-- To separate various bits of syntax with white space use 'space' instead
-- of @'txt' " "@. To output 'Outputable' Haskell entities like numbers use
-- 'atom'.

txt
  :: Text                       -- ^ 'Text' to output
  -> R ()
txt = spit False False

-- | Output 'Outputable' fragment of AST. This can be used to output numeric
-- literals and similar. Everything that doesn't have inner structure but
-- does have an 'Outputable' instance.

atom
  :: Outputable a
  => a
  -> R ()
atom = spit True False . T.pack . showOutputable

-- | Low-level non-public helper to define 'txt' and 'atom'.

spit
  :: Bool                       -- ^ Should we mark the line as dirty?
  -> Bool                       -- ^ Used during outputting of pending comments?
  -> Text                       -- ^ 'Text' to output
  -> R ()
spit dirty printingComments txt' = do
  requestedDel <- R (gets scRequestedDelimiter)
  case requestedDel of
    RequestedNewline -> do
      R . modify $ \sc -> sc
        { scRequestedDelimiter = RequestedNothing }
      if printingComments
        then newlineRaw
        else newline
    _ -> return ()
  R $ do
    i <- asks rcIndent
    c <- gets scColumn
    let spaces =
          if c < i
            then T.replicate (i - c) " "
            else bool mempty " " (requestedDel == RequestedSpace)
        indentedTxt = spaces <> txt'
    modify $ \sc -> sc
      { scBuilder = scBuilder sc <> fromText indentedTxt
      , scColumn = scColumn sc + T.length indentedTxt
      , scDirtyLine = scDirtyLine sc || dirty
      , scRequestedDelimiter = RequestedNothing
      , scLastCommentSpan =
          -- NOTE If there are pending comments, do not reset last comment
          -- location.
          if printingComments || (not . null . scPendingComments) sc
            then scLastCommentSpan sc
            else Nothing
      }

-- | This primitive /does not/ necessarily output a space. It just ensures
-- that the next thing that will be printed on the same line will be
-- separated by a single space from the previous output. Using this
-- combinator twice results in at most one space.
--
-- In practice this design prevents trailing white space and makes it hard
-- to output more than one delimiting space in a row, which is what we
-- usually want.

space :: R ()
space = R . modify $ \sc -> sc
  { scRequestedDelimiter =
      case scRequestedDelimiter sc of
        RequestedNothing -> RequestedSpace
        other -> other
  }

-- | Output a newline. First time 'newline' is used after some non-'newline'
-- output it gets inserted immediately. Second use of 'newline' does not
-- output anything but makes sure that the next non-white space output will
-- be prefixed by a newline. Using 'newline' more than twice in a row has no
-- effect. Also, using 'newline' at the very beginning has no effect, this
-- is to avoid leading whitespace.
--
-- Similarly to 'space', this design prevents trailing newlines and makes it
-- hard to output more than one blank newline in a row.

newline :: R ()
newline = do
  cs <- reverse <$> R (gets scPendingComments)
  case cs of
    [] -> newlineRaw
    ((position, _, _):_)  -> do
      case position of
        OnTheSameLine -> space
        OnNextLine -> newlineRaw
      R . forM_ cs $ \(_, indent, txt') ->
        let modRC rc = rc
              { rcIndent = indent
              }
            R m = do
              unless (T.null txt') $
                spit False True txt'
              newlineRaw
        in local modRC m
      R . modify $ \sc -> sc
        { scPendingComments = []
        }

-- | Low-level newline primitive. This one always just inserts a newline, no
-- hooks can be attached.

newlineRaw :: R ()
newlineRaw = R . modify $ \sc ->
  let requestedDel = scRequestedDelimiter sc
      builderSoFar = scBuilder sc
  in sc
    { scBuilder =
        case requestedDel of
          AfterNewline -> builderSoFar
          RequestedNewline -> builderSoFar
          VeryBeginning -> builderSoFar
          _ -> builderSoFar <> "\n"
    , scColumn = 0
    , scDirtyLine = False
    , scRequestedDelimiter =
        case scRequestedDelimiter sc of
          AfterNewline -> RequestedNewline
          RequestedNewline -> RequestedNewline
          VeryBeginning -> VeryBeginning
          _ -> AfterNewline
    }

-- | Check if the current line is “dirty”, that is, there is something on it
-- that can have comments attached to it.

isLineDirty :: R Bool
isLineDirty = R (gets scDirtyLine)

-- | Increase indentation level by one indentation step for the inner
-- computation. 'inci' should be used when a part of code must be more
-- indented relative to the parts outside of 'inci' in order for the output
-- to be valid Haskell. When layout is single-line there is no obvious
-- effect, but with multi-line layout correct indentation levels matter.

inci :: R () -> R ()
inci (R m) = R (local modRC m)
  where
    modRC rc = rc
      { rcIndent = rcIndent rc + indentStep
      }

-- | Set indentation level for the inner computation equal to current
-- column. This makes sure that the entire inner block is uniformly
-- \"shifted\" to the right. Only works (and makes sense) when enclosing
-- layout is multi-line.

sitcc :: R () -> R ()
sitcc (R m) = do
  requestedDel <- R (gets scRequestedDelimiter)
  i <- R (asks rcIndent)
  c <- R (gets scColumn)
  let modRC rc = rc
        { rcIndent = max i c + bool 0 1 (requestedDel == RequestedSpace)
        }
  vlayout (R m) . R $ do
    modify $ \sc -> sc
      { scRequestedDelimiter =
          case requestedDel of
            RequestedSpace -> RequestedNothing
            other -> other
      }
    local modRC m

-- | Set 'Layout' for internal computation.

enterLayout :: Layout -> R () -> R ()
enterLayout l (R m) = R (local modRC m)
  where
    modRC rc = rc
      { rcLayout = l
      }

-- | Do one or another thing depending on current 'Layout'.

vlayout
  :: R a                        -- ^ Single line
  -> R a                        -- ^ Multi line
  -> R a
vlayout sline mline = do
  l <- R (asks rcLayout)
  case l of
    SingleLine -> sline
    MultiLine -> mline

----------------------------------------------------------------------------
-- Special helpers for comment placement

-- | Register a comment line for outputting. It will be inserted right
-- before next newline. When the comment goes after something else on the
-- same line, a space will be inserted between preceding text and the
-- comment when necessary.

registerPendingCommentLine
  :: CommentPosition            -- ^ Comment position
  -> Text                       -- ^ 'Text' to output
  -> R ()
registerPendingCommentLine position txt' = R $ do
  i <- asks rcIndent
  modify $ \sc -> sc
    { scPendingComments = (position, i, txt') : scPendingComments sc
    }

-- | Drop elements that begin before or at the same place as given
-- 'SrcSpan'.

trimSpanStream
  :: RealSrcSpan                -- ^ Reference span
  -> R ()
trimSpanStream ref = do
  let leRef :: RealSrcSpan -> Bool
      leRef x = realSrcSpanStart x <= realSrcSpanStart ref
  R . modify $ \sc -> sc
    { scSpanStream = coerce (dropWhile leRef) (scSpanStream sc)
    }

-- | Get location of next element in AST.

nextEltSpan :: R (Maybe RealSrcSpan)
nextEltSpan = listToMaybe . coerce <$> R (gets scSpanStream)

-- | Pop a 'Comment' from the 'CommentStream' if given predicate is
-- satisfied and there are comments in the stream.

popComment
  :: (RealLocated Comment -> Bool)
  -> R (Maybe (RealLocated Comment))
popComment f = R $ do
  CommentStream cstream <- gets scCommentStream
  case cstream of
    [] -> return Nothing
    (x:xs) ->
      if f x
        then Just x <$ modify (\sc -> sc
               { scCommentStream = CommentStream xs
               })
        else return Nothing

-- | Get the first enclosing 'RealSrcSpan' that satisfies given predicate.

getEnclosingSpan
  :: (RealSrcSpan -> Bool)      -- ^ Predicate to use
  -> R (Maybe RealSrcSpan)
getEnclosingSpan f =
  listToMaybe . filter f <$> R (asks rcEnclosingSpans)

-- | Set 'RealSrcSpan' of enclosing span for the given computation.

withEnclosingSpan :: RealSrcSpan -> R () -> R ()
withEnclosingSpan spn (R m) = R (local modRC m)
  where
    modRC rc = rc
      { rcEnclosingSpans = spn : rcEnclosingSpans rc
      }

-- | Set span of last output comment.

setLastCommentSpan :: RealSrcSpan -> R ()
setLastCommentSpan spn = R . modify $ \sc -> sc
  { scLastCommentSpan = Just spn
  }

-- | Get span of last output comment.

getLastCommentSpan :: R (Maybe RealSrcSpan)
getLastCommentSpan = R (gets scLastCommentSpan)

----------------------------------------------------------------------------
-- Annotations

-- | For a given span return 'AnnKeywordId's associated with it.

getAnns
  :: SrcSpan
  -> R [AnnKeywordId]
getAnns spn = lookupAnns spn <$> R (asks rcAnns)

----------------------------------------------------------------------------
-- Helpers for braces

-- | Make the inner computation use braces around single-line layouts.

useBraces :: R () -> R ()
useBraces (R r) =  R (local (\i -> i {rcCanUseBraces = True}) r)

-- | Make the inner computation omit braces around single-line layouts.

dontUseBraces :: R () -> R ()
dontUseBraces (R r) =  R (local (\i -> i {rcCanUseBraces = False}) r)

-- | Return 'True' if we can use braces in this context.

canUseBraces :: R Bool
canUseBraces = R $ asks rcCanUseBraces

----------------------------------------------------------------------------
-- Constants

-- | Indentation step.

indentStep :: Int
indentStep = 2

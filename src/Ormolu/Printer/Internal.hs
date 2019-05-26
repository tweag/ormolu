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
  , spit
  , newline
  , modNewline
  , ensureIndent
  , inci
  , sitcc
  , Layout (..)
  , enterLayout
  , vlayout
    -- * Special helpers for comment placement
  , trimSpanStream
  , nextEltSpan
  , popComment
  , hasMoreComments
  , getIndent
  , setIndent
  , getEnclosingSpan
  , withEnclosingSpan
    -- * Annotations
  , getAnns
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Coerce
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Debug.Trace
import GHC
import Ormolu.Anns
import Ormolu.CommentStream
import Ormolu.SpanStream
import SrcLoc
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

----------------------------------------------------------------------------
-- The 'R' monad

-- | The 'R' monad hosts combinators that allow us to describe how to render
-- AST.

newtype R a = R (ReaderT RC (State SC) a)
  deriving (Functor, Applicative, Monad)

-- | Reader context of 'R'.

data RC = RC
  { rcIndent :: !Int
    -- ^ Indentation level, as the column index we need to start from after a
    -- newline if we break lines
  , rcLayout :: Layout
    -- ^ Current layout
  , rcRelaxedComments :: Bool
    -- ^ Whether to relax aligning rules for comments
  , rcDebug :: Bool
    -- ^ Whether to print debugging info as we go
  , rcEnclosingSpan :: Maybe RealSrcSpan
    -- ^ Span of enclosing element of AST
  , rcAnns :: Anns
    -- ^ Collection of annotations
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
  , scNewline :: R ()
    -- ^ What to render as newline
  }

-- | 'Layout' options.

data Layout
  = SingleLine                  -- ^ Put everything on single line
  | MultiLine                   -- ^ Use multiple lines
  deriving (Eq, Show)

-- | Run an 'R' monad.

runR
  :: Bool                       -- ^ Whether to print debugging info
  -> R ()                       -- ^ Monad to run
  -> SpanStream                 -- ^ Span stream
  -> CommentStream              -- ^ Comment stream
  -> Anns                       -- ^ Annotations
  -> Text                       -- ^ Resulting rendition
runR debug (R m) sstream cstream anns =
  TL.toStrict . toLazyText . scBuilder $ execState (runReaderT m rc) sc
  where
    rc = RC
      { rcIndent = 0
      , rcLayout = MultiLine
      , rcRelaxedComments = False
      , rcDebug = debug
      , rcEnclosingSpan = Nothing
      , rcAnns = anns
      }
    sc = SC
      { scColumn = 0
      , scBuilder = mempty
      , scSpanStream = sstream
      , scCommentStream = cstream
      , scNewline = newlineRaw
      }

----------------------------------------------------------------------------
-- Internal functions

-- | Grow current output by appending given 'Text' to it, which may not
-- contain newlines.

spit :: Text -> R ()
spit x = do
  traceR "spit_before" (Just x)
  R . modify $ \sc -> sc
    { scBuilder = scBuilder sc <> fromText x
    , scColumn = scColumn sc + T.length x
    }
  traceR "spit_after" Nothing

-- | Output a newline.

newline :: R ()
newline = do
  n <- R (gets scNewline)
  R . modify $ \sc -> sc
    { scNewline = newlineRaw
    }
  n

-- | Low-level newline primitive. This one always just inserts a newline, no
-- hooks can be attached.

newlineRaw :: R ()
newlineRaw = do
  traceR "newline_before" (Just "\n")
  R . modify $ \sc -> sc
    { scBuilder = scBuilder sc <> "\n"
    , scColumn = 0
    }
  traceR "newline_after" Nothing

-- | The 'modNewline' function can be used to alter what will be inserted as
-- a newline. This is used to output comments following an element of AST
-- because we cannot output comments immediately, e.g. because we need to
-- close parentheses first, etc.
--
-- 'newline' auto-resets its modifications so the changes introduced with
-- 'modNewline' only have effect once.
--
-- The argument of the call-back is the version of 'newline' built so far.

modNewline :: (R () -> R ()) -> R ()
modNewline f = R $ do
  old <- gets scNewline
  modify $ \sc -> sc
    { scNewline = f old
    }

-- | Ensure that indentation level is satisfied. Insert correct number of
-- spaces if it isn't.

ensureIndent :: R ()
ensureIndent = do
  traceR "ensure_indent" Nothing
  i <- R (asks rcIndent)
  c <- R (gets scColumn)
  when (c < i) $
    spit (T.replicate (i - c) " ")

-- | Increase indentation level by one indentation step for the inner
-- computation. 'inci' should be used when a part of code must be more
-- indented relative to the parts outside of 'inci' in order for the output
-- to be valid Haskell. When layout is single-line there is no obvious
-- effect, but with multi-line layout correct indentation levels matter.

inci :: R () -> R ()
inci m' = do
  traceR "inci_before" Nothing
  let R m = traceR "inci_inside" Nothing >> m'
      modRC rc = rc
        { rcIndent = rcIndent rc + indentStep
        }
  R (local modRC m)
  traceR "inci_ended" Nothing

-- | Set indentation level for the inner computation equal to current
-- column. This makes sure that the entire inner block is uniformly
-- \"shifted\" to the right. Only works (and makes sense) when enclosing
-- layout is multi-line.

sitcc :: R () -> R ()
sitcc m' = do
  traceR "sitcc_before" Nothing
  i <- R (asks rcIndent)
  c <- R (gets scColumn)
  let R m = traceR "sitcc_inside" Nothing >> m'
      modRC rc = rc
        { rcIndent = max i c
        }
  vlayout m' (R (local modRC m))
  traceR "sitcc_ended" Nothing

-- | Set 'Layout' for internal computation.

enterLayout :: Layout -> R () -> R ()
enterLayout l (R m) = do
  let label =
        case l of
          SingleLine -> "single_line"
          MultiLine -> "multi_line"
  traceR ("lstart_" ++ label) Nothing
  let modRC rc = rc
        { rcLayout = l
        }
  x <- R (local modRC m)
  traceR ("lend_" ++ label) Nothing
  return x

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

-- | Return 'True' if there are more comments in the 'CommentStream'.

hasMoreComments :: R Bool
hasMoreComments = R $ do
  CommentStream cstream <- gets scCommentStream
  (return . not . null) cstream

-- | Current indentation level.

getIndent :: R Int
getIndent = R (asks rcIndent)

-- | Set indentation level for the given computation.

setIndent :: Int -> R () -> R ()
setIndent i m' = do
  traceR "set_indent_before" Nothing
  let R m = traceR "set_indent_inside" Nothing >> m'
      modRC rc = rc
        { rcIndent = i
        }
  R (local modRC m)
  traceR "set_indent_after" Nothing

-- | Get 'RealSrcSpan' of enclosing span, if any.

getEnclosingSpan :: R (Maybe RealSrcSpan)
getEnclosingSpan = R (asks rcEnclosingSpan)

-- | Set 'RealSrcSpan' of enclosing span for the given computation.

withEnclosingSpan :: RealSrcSpan -> R () -> R ()
withEnclosingSpan spn (R m) = do
  let modRC rc = rc
        { rcEnclosingSpan = Just spn
        }
  R (local modRC m)

----------------------------------------------------------------------------
-- Annotations

-- | For a given span return 'AnnKeywordId's associated with it.

getAnns
  :: SrcSpan
  -> R [AnnKeywordId]
getAnns spn = lookupAnns spn <$> R (asks rcAnns)

----------------------------------------------------------------------------
-- Debug helpers

-- | Tracing helper.

traceR
  :: String                     -- ^ Label
  -> Maybe Text                 -- ^ Output, if any
  -> R ()
traceR label moutput = R $ do
  debug <- asks rcDebug
  i <- asks rcIndent
  c <- gets scColumn
  when debug . traceM . concat $
    [ replicate i ' '
    , unwords [ label ++ ":"
              , "i=" ++ show i
              , "c=" ++ show c
              ]
    ] ++ case moutput of
           Nothing -> []
           Just output -> [" out=" ++ show output]

----------------------------------------------------------------------------
-- Constants

-- | Indentation step.

indentStep :: Int
indentStep = 2

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
  , modNewline
  , isNewlineModified
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
import Data.Bool (bool)
import Data.Coerce
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Debug.Trace
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

-- | Reader context of 'R'.

data RC = RC
  { rcIndent :: !Int
    -- ^ Indentation level, as the column index we need to start from after a
    -- newline if we break lines
  , rcLayout :: Layout
    -- ^ Current layout
  , rcDebug :: Bool
    -- ^ Whether to print debugging info as we go
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
  , scNewline :: Maybe (R ())
    -- ^ What to render as newline, or 'Nothing' ('newlineRaw' will be used)
  , scDirtyLine :: !Bool
    -- ^ Whether the current line is “dirty”
  , scSpace :: !Bool
    -- ^ Whether to output a space before the next output
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
      , rcDebug = debug
      , rcEnclosingSpans = []
      , rcAnns = anns
      , rcCanUseBraces = False
      }
    sc = SC
      { scColumn = 0
      , scBuilder = mempty
      , scSpanStream = sstream
      , scCommentStream = cstream
      , scNewline = Nothing
      , scDirtyLine = False
      , scSpace = False
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
  :: Text -- ^ 'Text' to output
  -> R ()
txt = spit False

-- | Output 'Outputable' fragment of AST. This can be used to output numeric
-- literals and similar. Everything that doesn't have inner structure but
-- does have an 'Outputable' instance.

atom
  :: Outputable a
  => a
  -> R ()
atom = spit True . T.pack . showOutputable

-- | Low-level non-public helper to define 'txt' and 'atom'.

spit :: Bool -> Text -> R ()
spit dirty x' = do
  i <- R (asks rcIndent)
  c <- R (gets scColumn)
  needsSpace <- R (gets scSpace)
  let spaces =
        if (c < i)
          then T.replicate (i - c) " "
          else bool mempty " " needsSpace
      x = spaces <> x'
  traceR "spit_before" (Just x)
  R . modify $ \sc -> sc
    { scBuilder = scBuilder sc <> fromText x
    , scColumn = scColumn sc + T.length x
    , scDirtyLine = scDirtyLine sc || dirty
    , scSpace = False
    }
  traceR "spit_after" Nothing

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
  { scSpace = True
  }

-- | Output a newline.

newline :: R ()
newline = do
  n <- R (gets scNewline)
  R . modify $ \sc -> sc
    { scNewline = Nothing
    }
  fromMaybe newlineRaw n

-- | Low-level newline primitive. This one always just inserts a newline, no
-- hooks can be attached.

newlineRaw :: R ()
newlineRaw = do
  traceR "newline_before" (Just "\n")
  R . modify $ \sc -> sc
    { scBuilder = scBuilder sc <> "\n"
    , scColumn = 0
    , scDirtyLine = False
    , scSpace = False
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
    { scNewline = Just $ f (fromMaybe newlineRaw old)
    }

-- | Check if newline is in modified state.

isNewlineModified :: R Bool
isNewlineModified = isJust <$> R (gets scNewline)

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
  needsSpace <- R (gets scSpace)
  let R m = traceR "sitcc_inside" Nothing >> m'
      modRC rc = rc
        { rcIndent = max i c + bool 0 1 needsSpace
        }
  vlayout m' . R $ do
    modify $ \sc -> sc { scSpace = False }
    local modRC m
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

-- | Get the first enclosing 'RealSrcSpan' that satisfies given predicate.

getEnclosingSpan
  :: (RealSrcSpan -> Bool)      -- ^ Predicate to use
  -> R (Maybe RealSrcSpan)
getEnclosingSpan f =
  listToMaybe . filter f <$> R (asks rcEnclosingSpans)

-- | Set 'RealSrcSpan' of enclosing span for the given computation.

withEnclosingSpan :: RealSrcSpan -> R () -> R ()
withEnclosingSpan spn (R m) = do
  let modRC rc = rc
        { rcEnclosingSpans = spn : rcEnclosingSpans rc
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

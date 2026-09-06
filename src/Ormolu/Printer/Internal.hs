{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | In most cases, import "Ormolu.Printer.Combinators" instead; these
-- functions are the low-level building blocks and should not be used on
-- their own. The 'R' monad is re-exported from "Ormolu.Printer.Combinators"
-- as well.
module Ormolu.Printer.Internal
  ( -- * The 'R' monad
    R,
    runR,

    -- * Internal functions
    txt,
    atom,
    space,
    newline,
    newlineLiteral,
    askSourceType,
    askModuleFixityMap,
    askDebug,
    inci,
    sitcc,
    Layout (..),
    enterLayout,
    vlayout,
    getLayout,

    -- * Helpers for braces
    useBraces,
    dontUseBraces,
    canUseBraces,

    -- * Special helpers for comment placement
    CommentPosition (..),
    registerPendingCommentLine,
    withAnchorMap,
    getCommentsAnchoredWithin,
    getCommentsBefore,
    getEnclosingSpan,
    withEnclosingSpan,
    thisLineSpans,

    -- * Stateful markers
    LastEmitted (..),
    lastEmittedSpan,
    setLastEmitted,
    getLastEmitted,

    -- * Haddocks
    HaddockStyle (..),
    lookupHaddockText,

    -- * Recording comment placement
    recordCommentPlacement,
    recordVisitedSpan,

    -- * Extensions
    isExtensionEnabled,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bool (bool)
import Data.Choice (Choice)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder
import GHC.Data.EnumSet (EnumSet)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.LanguageExtensions.Type
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (Outputable)
import Ormolu.Comments.Anchor (AnchorMap, commentsAnchoredWithin, commentsBefore)
import Ormolu.Config (SourceType (..))
import Ormolu.Fixity (ModuleFixityMap)
import Ormolu.Parser.CommentStream
import Ormolu.Printer.CommentPlacement
import Ormolu.Utils (showOutputable)

----------------------------------------------------------------------------
-- The 'R' monad

-- | The 'R' monad hosts combinators that allow us to describe how to render
-- the AST.
newtype R a = R (ReaderT RC (State SC) a)
  deriving (Functor, Applicative, Monad)

-- | Reader context of 'R'. This should be used when we control rendering by
-- enclosing certain expressions with wrappers.
data RC = RC
  { -- | Indentation level, as the column index we need to start from after
    -- a newline if we break lines
    rcIndent :: !Int,
    -- | Current layout
    rcLayout :: Layout,
    -- | Spans of enclosing elements of the AST
    rcEnclosingSpans :: [RealSrcSpan],
    -- | Whether the last expression in the layout can use braces
    rcCanUseBraces :: Bool,
    -- | Enabled extensions
    rcExtensions :: EnumSet Extension,
    -- | Whether the source is a signature or a regular module
    rcSourceType :: SourceType,
    -- | Module fixity map
    rcModuleFixityMap :: ModuleFixityMap,
    -- | Whether to print out debug information during printing
    rcDebug :: !(Choice "debug"),
    -- | Source text of the module's Haddocks
    rcHaddockText :: HaddockText
  }

-- | State context of 'R'.
data SC = SC
  { -- | Index of the next column to render
    scColumn :: !Int,
    -- | Indentation level that was used for the current line
    scIndent :: !Int,
    -- | Rendered source code so far
    scBuilder :: Builder,
    -- | Spans of atoms that have been printed on the current line so far
    scThisLineSpans :: [RealSrcSpan],
    -- | Comments that have not been emitted yet, by the element they are
    -- attached to
    scAnchorMap :: AnchorMap,
    -- | Pending comment lines (in reverse order) to be inserted before the
    -- next newline
    scPendingComments :: ![(CommentPosition, Text)],
    -- | Whether to output a space before the next output
    scRequestedDelimiter :: !RequestedDelimiter,
    -- | What was emitted last, used both for preserving blank lines from
    -- the input and for recognizing runs of comments
    scLastEmitted :: !LastEmitted,
    -- | Comment placement decisions made so far, in reverse order
    scCommentPlacements :: [CommentPlacement],
    -- | Spans of the elements the printer has entered, in reverse order
    scVisitedSpans :: [RealSrcSpan]
  }

-- | Make sure the next output is delimited by one of the following.
data RequestedDelimiter
  = -- | A space
    RequestedSpace
  | -- | A newline
    RequestedNewline
  | -- | Nothing
    RequestedNothing
  | -- | We just output a newline
    AfterNewline
  | -- | We haven't printed anything yet
    VeryBeginning
  deriving (Eq, Show)

-- | 'Layout' options.
data Layout
  = -- | Put everything on a single line
    SingleLine
  | -- | Use multiple lines
    MultiLine
  deriving (Eq, Show)

-- | Modes for rendering pending comments.
data CommentPosition
  = -- | Put the comment on the same line
    OnTheSameLine
  | -- | Put the comment on the next line
    OnNextLine
  deriving (Eq, Show)

-- | Run 'R' monad.
runR ::
  -- | Monad to run
  R () ->
  -- | Comments, attached to the elements they belong to
  AnchorMap ->
  -- | Whether the source is a signature or a regular module
  SourceType ->
  -- | Enabled extensions
  EnumSet Extension ->
  -- | Module fixity map
  ModuleFixityMap ->
  -- | Whether to print out debug information during printing
  Choice "debug" ->
  -- | Source text of the module's Haddocks
  HaddockText ->
  -- | The rendition, the comment placement decisions that were made along
  -- the way, and the spans of the elements that were entered
  (Text, [CommentPlacement], [RealSrcSpan])
runR (R m) anchorMap sourceType extensions moduleFixityMap debug haddockText =
  ( TL.toStrict . toLazyText . scBuilder $ finalSc,
    reverse (scCommentPlacements finalSc),
    reverse (scVisitedSpans finalSc)
  )
  where
    finalSc = execState (runReaderT m rc) sc
    rc =
      RC
        { rcIndent = 0,
          rcLayout = MultiLine,
          rcEnclosingSpans = [],
          rcCanUseBraces = False,
          rcExtensions = extensions,
          rcSourceType = sourceType,
          rcModuleFixityMap = moduleFixityMap,
          rcDebug = debug,
          rcHaddockText = haddockText
        }
    sc =
      SC
        { scColumn = 0,
          scIndent = 0,
          scBuilder = mempty,
          scThisLineSpans = [],
          scAnchorMap = anchorMap,
          scPendingComments = [],
          scRequestedDelimiter = VeryBeginning,
          scLastEmitted = LastEmittedOther,
          scCommentPlacements = [],
          scVisitedSpans = []
        }

----------------------------------------------------------------------------
-- Internal functions

-- | Type of the thing to output. Influences the primary low-level rendering
-- function 'spit'.
data SpitType
  = -- | Simple opaque text that breaks comment series.
    SimpleText
  | -- | An atom that typically has span information in the AST and can
    -- have comments attached to it.
    Atom
  | -- | Used for rendering comment lines.
    CommentPart
  deriving (Show, Eq)

-- | Output a fixed 'Text' fragment. The argument may not contain any line
-- breaks. 'txt' is used to output all sorts of “fixed” bits of syntax like
-- keywords and pipes @|@ in functional dependencies.
--
-- To separate various bits of syntax with white space use 'space' instead
-- of @'txt' " "@. To output 'Outputable' Haskell entities like numbers use
-- 'atom'.
txt ::
  -- | 'Text' to output
  Text ->
  R ()
txt = spit SimpleText

-- | Output an 'Outputable' fragment of the AST. This can be used to output
-- numeric literals and similar: anything that doesn't have inner structure
-- but does have an 'Outputable' instance.
atom ::
  (Outputable a) =>
  a ->
  R ()
atom = spit Atom . T.pack . showOutputable

-- | Low-level non-public helper to define 'txt' and 'atom'.
spit ::
  -- | Type of the thing to spit
  SpitType ->
  -- | 'Text' to output
  Text ->
  R ()
spit _ "" = return ()
spit stype text = do
  requestedDel <- R (gets scRequestedDelimiter)
  case requestedDel of
    RequestedNewline -> do
      R . modify $ \sc ->
        sc
          { scRequestedDelimiter = RequestedNothing
          }
      case stype of
        CommentPart -> newlineRaw
        _ -> newline
    _ -> return ()
  R $ do
    i <- asks rcIndent
    c <- gets scColumn
    closestEnclosing <- asks (listToMaybe . rcEnclosingSpans)
    let indentedTxt = spaces <> text
        spaces = T.replicate spacesN " "
        spacesN =
          if c == 0
            then i
            else bool 0 1 (requestedDel == RequestedSpace)
    modify $ \sc ->
      sc
        { scBuilder = scBuilder sc <> fromText indentedTxt,
          scColumn = scColumn sc + T.length indentedTxt,
          scIndent =
            if c == 0
              then i
              else scIndent sc,
          scThisLineSpans =
            let xs = scThisLineSpans sc
             in case stype of
                  Atom -> case closestEnclosing of
                    Nothing -> xs
                    Just x -> x : xs
                  _ -> xs,
          scRequestedDelimiter = RequestedNothing,
          scLastEmitted =
            -- If there are pending comments, do not reset last comment
            -- location.
            if (stype == CommentPart) || (not . null . scPendingComments) sc
              then scLastEmitted sc
              else LastEmittedOther
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
space = R . modify $ \sc ->
  sc
    { scRequestedDelimiter = case scRequestedDelimiter sc of
        RequestedNothing -> RequestedSpace
        other -> other
    }

-- | Output a newline. The first time 'newline' is used after some
-- non-'newline' output, it gets inserted immediately. The second use of
-- 'newline' does not output anything but makes sure that the next
-- non-whitespace output will be prefixed by a newline. Using 'newline' more
-- than twice in a row has no effect. Also, using 'newline' at the very
-- beginning has no effect; this is to avoid leading whitespace.
--
-- Similarly to 'space', this design prevents trailing newlines and makes it
-- hard to output more than one blank newline in a row.
newline :: R ()
newline = do
  lineIndent <- R (gets scIndent)
  logicalIndent <- R (asks rcIndent)
  -- A trailing comment block spills onto the lines below the code it
  -- trails. Those lines take the indentation of the line the block started
  -- on, unless the construct being printed is indented further, in which
  -- case they follow it: dropping to the start of the line would put the
  -- rest of a block comment outside the declaration it was written in.
  let indent = max lineIndent logicalIndent
  cs <- reverse <$> R (gets scPendingComments)
  case cs of
    [] -> newlineRaw
    ((position, _) : _) -> do
      case position of
        OnTheSameLine -> space
        OnNextLine -> newlineRaw
      R . forM_ cs $ \(_, text) ->
        let modRC rc =
              rc
                { rcIndent = indent
                }
            R m = do
              unless (T.null text) $
                spit CommentPart text
              newlineRaw
         in local modRC m
      R . modify $ \sc ->
        sc
          { scPendingComments = []
          }

-- | Low-level newline primitive. This one always just inserts a newline, no
-- hooks can be attached.
newlineRaw :: R ()
newlineRaw = R . modify $ \sc ->
  let requestedDel = scRequestedDelimiter sc
      builderSoFar = scBuilder sc
   in sc
        { scBuilder = case requestedDel of
            AfterNewline -> builderSoFar
            RequestedNewline -> builderSoFar
            VeryBeginning -> builderSoFar
            _ -> builderSoFar <> "\n",
          scColumn = 0,
          scIndent = 0,
          scThisLineSpans = [],
          scRequestedDelimiter = case scRequestedDelimiter sc of
            AfterNewline -> RequestedNewline
            RequestedNewline -> RequestedNewline
            VeryBeginning -> VeryBeginning
            _ -> AfterNewline
        }

-- | Insert a literal newline without modifying the internal state of the
-- printer. This is to be used in exceptional cases, e.g. for printing
-- multiline string literals.
newlineLiteral :: R ()
newlineLiteral = R . modify $ \sc ->
  sc
    { scBuilder = scBuilder sc <> "\n",
      scColumn = 0,
      scIndent = 0,
      scThisLineSpans = [],
      scRequestedDelimiter = AfterNewline
    }

-- | Return the source type.
askSourceType :: R SourceType
askSourceType = R (asks rcSourceType)

-- | Retrieve the module fixity map.
askModuleFixityMap :: R ModuleFixityMap
askModuleFixityMap = R (asks rcModuleFixityMap)

-- | Retrieve whether we should print out certain debug information while
-- printing.
askDebug :: R (Choice "debug")
askDebug = R (asks rcDebug)

inciBy :: Int -> R () -> R ()
inciBy step (R m) = R (local modRC m)
  where
    modRC rc =
      rc
        { rcIndent = rcIndent rc + step
        }

-- | Increase the indentation level by one indentation step for the inner
-- computation. 'inci' should be used when a piece of code must be more
-- indented relative to the parts outside of 'inci' in order for the output
-- to be valid Haskell. With single-line layout there is no visible effect,
-- but with multi-line layout correct indentation levels matter.
inci :: R () -> R ()
inci = inciBy indentStep

-- | Set the indentation level for the inner computation equal to the
-- current column. This makes sure that the entire inner block is uniformly
-- \"shifted\" to the right.
sitcc :: R () -> R ()
sitcc (R m) = do
  requestedDel <- R (gets scRequestedDelimiter)
  i <- R (asks rcIndent)
  c <- R (gets scColumn)
  let modRC rc =
        rc
          { rcIndent = max i (c + bool 0 1 (requestedDel == RequestedSpace))
          }
  R (local modRC m)

-- | Set the 'Layout' for the inner computation.
enterLayout :: Layout -> R () -> R ()
enterLayout l (R m) = R (local modRC m)
  where
    modRC rc =
      rc
        { rcLayout = l
        }

-- | Do one thing or another depending on the current 'Layout'.
vlayout ::
  -- | Single line
  R a ->
  -- | Multi line
  R a ->
  R a
vlayout sline mline = do
  l <- getLayout
  case l of
    SingleLine -> sline
    MultiLine -> mline

-- | Get the current 'Layout'.
getLayout :: R Layout
getLayout = R (asks rcLayout)

----------------------------------------------------------------------------
-- Special helpers for comment placement

-- | Register a comment line for output. It will be inserted right before
-- the next newline. When the comment goes after something else on the same
-- line, a space will be inserted between the preceding text and the comment
-- when necessary.
registerPendingCommentLine ::
  -- | Comment position
  CommentPosition ->
  -- | 'Text' to output
  Text ->
  R ()
registerPendingCommentLine position text = R $ do
  modify $ \sc ->
    sc
      { scPendingComments = (position, text) : scPendingComments sc
      }

-- | Claim comments from the anchor map, storing what is left.
withAnchorMap :: (AnchorMap -> (a, AnchorMap)) -> R a
withAnchorMap f = R . state $ \sc ->
  let (a, am) = f (scAnchorMap sc)
   in (a, sc {scAnchorMap = am})

-- | Get the comments that will be printed before the element at the given
-- span. Like 'getCommentsAnchoredWithin', this only looks; it does not
-- claim.
getCommentsBefore :: RealSrcSpan -> R [LComment]
getCommentsBefore spn = withAnchorMap (\am -> (commentsBefore spn am, am))

-- | Get the comments attached to the element at the given span, or to
-- anything inside it.
--
-- This only looks; it does not claim. The layout decisions that ask this
-- run before the comments are emitted, and claiming here would leave
-- nothing for the printer to emit later.
getCommentsAnchoredWithin :: RealSrcSpan -> R [LComment]
getCommentsAnchoredWithin region =
  withAnchorMap (\am -> (commentsAnchoredWithin region am, am))

-- | Get the immediately enclosing 'RealSrcSpan'.
getEnclosingSpan :: R (Maybe RealSrcSpan)
getEnclosingSpan = getEnclosingSpanWhere (const True)

-- | Get the first enclosing 'RealSrcSpan' that satisfies the given
-- predicate.
getEnclosingSpanWhere ::
  -- | Predicate to use
  (RealSrcSpan -> Bool) ->
  R (Maybe RealSrcSpan)
getEnclosingSpanWhere f =
  find f <$> R (asks rcEnclosingSpans)

-- | Set the 'RealSrcSpan' of the enclosing span for the given computation.
withEnclosingSpan :: RealSrcSpan -> R () -> R ()
withEnclosingSpan spn (R m) = R (local modRC m)
  where
    modRC rc =
      rc
        { rcEnclosingSpans = spn : rcEnclosingSpans rc
        }

-- | Get spans on this line so far.
thisLineSpans :: R [RealSrcSpan]
thisLineSpans = R (gets scThisLineSpans)

----------------------------------------------------------------------------
-- Stateful markers

-- | What the printer emitted last, and where it came from in the input.
--
-- This is about spacing, not about attachment: it is what lets a blank line
-- in the input be preserved in the output, and what lets a run of comment
-- lines be recognized as one. Statements are tracked for the first of those
-- reasons, Haddocks for the second.
data LastEmitted
  = -- | Nothing yet, or ordinary code
    LastEmittedOther
  | -- | A comment occupying the given span of the input
    LastEmittedComment RealSrcSpan
  | -- | A Haddock occupying the given span of the input
    LastEmittedHaddock RealSrcSpan
  | -- | A statement of a layout block occupying the given span
    LastEmittedStatement RealSrcSpan
  deriving (Eq, Show)

-- | Where the last emitted thing came from in the input, if it came from
-- anywhere in particular.
lastEmittedSpan :: LastEmitted -> Maybe RealSrcSpan
lastEmittedSpan = \case
  LastEmittedOther -> Nothing
  LastEmittedComment s -> Just s
  LastEmittedHaddock s -> Just s
  LastEmittedStatement s -> Just s

-- | Record what was emitted last.
setLastEmitted :: LastEmitted -> R ()
setLastEmitted lastEmitted = R . modify $ \sc ->
  sc
    { scLastEmitted = lastEmitted
    }

-- | Report what was emitted last.
getLastEmitted :: R LastEmitted
getLastEmitted = R (gets scLastEmitted)

-- | Haddock string style, i.e. the trigger a Haddock is rendered with.
data HaddockStyle
  = -- | @-- |@
    Pipe
  | -- | @-- ^@
    Caret
  | -- | @-- *@
    Asterisk Int
  | -- | @-- $@
    Named String

-- | The source text of the Haddock at the given span, if it is one of the
-- module's Haddocks. See 'Ormolu.Parser.CommentStream.HaddockText'.
lookupHaddockText :: RealSrcSpan -> R (Maybe Comment)
lookupHaddockText spn = R (asks (M.lookup spn . rcHaddockText))

----------------------------------------------------------------------------
-- Recording comment placement

-- | Record the fact that a comment was rendered in a particular slot.
--
-- Every code path that emits a comment has to call this. What is recorded
-- here is what "Ormolu.Comments.Invariants" checks the input's comments
-- against, so a comment emitted without being recorded is reported as
-- dropped and Ormolu refuses to format the file.
recordCommentPlacement :: CommentPlacement -> R ()
recordCommentPlacement placement = R . modify $ \sc ->
  sc
    { scCommentPlacements = placement : scCommentPlacements sc
    }

-- | Record that the printer entered the element with the given span.
--
-- Not every span in the AST is entered: the printer renders plenty of
-- syntax with 'txt' rather than through 'Ormolu.Printer.Combinators.located',
-- so a @where@ clause, for instance, has a span but is never entered. A
-- comment can only be attached to an element that is entered, because
-- entering it is the only moment at which the comment could be emitted.
recordVisitedSpan :: RealSrcSpan -> R ()
recordVisitedSpan spn = R . modify $ \sc ->
  sc
    { scVisitedSpans = spn : scVisitedSpans sc
    }

----------------------------------------------------------------------------
-- Helpers for braces

-- | Make the inner computation use braces around single-line layouts.
useBraces :: R () -> R ()
useBraces (R r) = R (local (\i -> i {rcCanUseBraces = True}) r)

-- | Make the inner computation omit braces around single-line layouts.
dontUseBraces :: R () -> R ()
dontUseBraces (R r) = R (local (\i -> i {rcCanUseBraces = False}) r)

-- | Return 'True' if we can use braces in this context.
canUseBraces :: R Bool
canUseBraces = R (asks rcCanUseBraces)

----------------------------------------------------------------------------
-- Constants

-- | Indentation step.
indentStep :: Int
indentStep = 2

----------------------------------------------------------------------------
-- Extensions

isExtensionEnabled :: Extension -> R Bool
isExtensionEnabled ext = R . asks $ EnumSet.member ext . rcExtensions

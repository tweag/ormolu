{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Printing combinators. The definitions here are presented in such an
-- order that you can just read through the Haddocks, and by the end of the
-- file you should have a pretty good idea of how to program rendering logic.
module Ormolu.Printer.Combinators
  ( -- * The 'R' monad
    R,
    runR,
    getEnclosingSpan,
    getCommentsAnchoredWithin,
    getCommentsBefore,
    isExtensionEnabled,

    -- * Combinators

    -- ** Basic
    txt,
    atom,
    space,
    newline,
    newlineLiteral,
    inci,
    inciIf,
    askSourceType,
    askModuleFixityMap,
    askDebug,
    located,
    locatedEmpty,
    located',
    switchLayout,
    switchLayoutWithEnclosingComments,
    enterLayout,
    Layout (..),
    vlayout,
    getLayout,
    breakpoint,
    breakpoint',

    -- ** Formatting lists
    sep,
    sepSemi,
    sepSemi',
    canUseBraces,
    useBraces,
    dontUseBraces,

    -- ** Wrapping
    BracketStyle (..),
    sitcc,
    backticks,
    banana,
    braces,
    brackets,
    parens,
    parensHash,
    pragmaBraces,
    pragma,

    -- ** Literals
    comma,
    commaDel,

    -- ** Stateful markers
    LastEmitted (..),
    lastEmittedSpan,
    HaddockStyle (..),
    setLastEmitted,
    getLastEmitted,

    -- ** Haddocks
    lookupHaddockText,

    -- ** Placement
    Placement (..),
    placeHanging,
  )
where

import Control.Monad
import Data.List (intersperse)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import GHC.Data.Strict qualified as Strict
import GHC.Parser.Annotation
import GHC.Types.SrcLoc
import Ormolu.Printer.Comments
import Ormolu.Printer.Internal
import Ormolu.Utils (combineSrcSpans')

----------------------------------------------------------------------------
-- Basic

-- | Indent the inner expression if the first argument is 'True'.
inciIf ::
  -- | Whether to indent
  Bool ->
  -- | The expression to indent
  R () ->
  R ()
inciIf b m = if b then inci m else m

-- | Enter a 'GenLocated' entity. This combinator handles outputting comments
-- and sets the layout (single-line vs multi-line) for the inner computation.
-- Roughly, the rule for using 'located' is that every time there is a
-- 'Located' wrapper, it should be “discharged” with a corresponding
-- 'located' invocation.
located ::
  (HasLoc l) =>
  -- | Thing to enter
  GenLocated l a ->
  -- | How to render the inner value
  (a -> R ()) ->
  R ()
located (L l' a) f = case locA l' of
  UnhelpfulSpan _ -> f a
  RealSrcSpan l _ -> do
    recordVisitedSpan l
    spitPrecedingComments l
    withEnclosingSpan l $
      switchLayout [RealSrcSpan l Strict.Nothing] (f a)
    spitFollowingComments l

-- | Give an empty bracketed construct something for a comment written
-- inside it to attach to.
--
-- Brackets are rendered with 'txt', so an empty export or import list, an
-- empty @[]@ or a record with no fields contains no element at all. A
-- comment written between the brackets would be attached to whatever
-- encloses them and rendered outside them, so a zero-width element is
-- entered at the opening bracket instead.
locatedEmpty ::
  -- | Span of the empty construct
  SrcSpan ->
  R ()
locatedEmpty l =
  let loc = srcSpanStart l
   in located (L (mkSrcSpan loc loc) ()) pure

-- | A version of 'located' with the arguments flipped.
located' ::
  (HasLoc l) =>
  -- | How to render the inner value
  (a -> R ()) ->
  -- | Thing to enter
  GenLocated l a ->
  R ()
located' = flip located

-- | Set the layout according to the combination of the given 'SrcSpan's,
-- together with the spans of the comments that belong inside them.
--
-- Comments count towards the layout: a construct that would fit on one line
-- has to be broken up anyway if a comment was written inside it, or the
-- comment would swallow whatever follows it on the line.
--
-- 'located' calls this for you. Call it directly only when the layout has
-- to come from something the GHC AST has no 'Located' wrapper for, such as
-- the combined span of several elements; that is rare.
--
-- Given an empty list and no comments, this function will set the layout to
-- single-line.
switchLayout ::
  -- | Span that controls layout
  [SrcSpan] ->
  -- | Computation to run with changed layout
  R () ->
  R ()
switchLayout spans' m = do
  csSpans <- commentSpansIn (combineSrcSpans' <$> NE.nonEmpty spans')
  enterLayout (spansLayout (spans' <> csSpans)) m

-- | Like 'switchLayout', but the comments are looked for in the enclosing
-- element rather than in the given spans.
--
-- This is what a bracketed construct needs. In
--
-- > ( -- c
-- >   x
-- > )
--
-- the comment sits between the bracket and @x@, so it is inside neither of
-- them, and the parentheses would be put on one line despite it. Widening
-- the question to the enclosing element catches it. Do not reach for this
-- elsewhere: it is deliberately coarser than 'switchLayout', and applying
-- it where the enclosing element is large would let one comment break every
-- layout decision inside it.
switchLayoutWithEnclosingComments ::
  -- | Span that controls layout
  [SrcSpan] ->
  -- | Computation to run with changed layout
  R () ->
  R ()
switchLayoutWithEnclosingComments spans' m = do
  enclosing <- getEnclosingSpan
  csSpans <- commentSpansIn (flip RealSrcSpan Strict.Nothing <$> enclosing)
  enterLayout (spansLayout (spans' <> csSpans)) m

-- | The spans of the comments that belong inside the given region: both
-- attached to something in it and written inside it.
--
-- Both halves are needed. Without the first, a comment anywhere in a
-- declaration would force every layout decision inside that declaration to
-- multi-line. Without the second, a comment trailing an element would force
-- that element itself to be broken up.
--
-- Haddocks are not consulted here. They do not travel in the anchor map,
-- and their spans sit where the author wrote them rather than where they
-- will be printed, which is the wrong question; see
-- 'Ormolu.Printer.Meat.Common.multiLineIfDocumented'.
commentSpansIn :: Maybe SrcSpan -> R [SrcSpan]
commentSpansIn = \case
  Just (RealSrcSpan region _) -> do
    comments <- getCommentsAnchoredWithin region
    pure
      [ RealSrcSpan spn Strict.Nothing
      | L spn _ <- comments,
        region `containsSpan` spn
      ]
  _ -> pure []

-- | Which layout do the combined spans result in?
spansLayout :: [SrcSpan] -> Layout
spansLayout = \case
  [] -> SingleLine
  (x : xs) ->
    if isOneLineSpan (foldr combineSrcSpans x xs)
      then SingleLine
      else MultiLine

-- | Insert a space if the enclosing layout is single-line, or a newline if
-- it is multi-line.
--
-- > breakpoint = vlayout space newline
breakpoint :: R ()
breakpoint = vlayout space newline

-- | Similar to 'breakpoint', but outputs nothing in the case of single-line
-- layout.
--
-- > breakpoint' = vlayout (return ()) newline
breakpoint' :: R ()
breakpoint' = vlayout (return ()) newline

----------------------------------------------------------------------------
-- Formatting lists

-- | Render a collection of elements, inserting a separator between them.
sep ::
  -- | Separator
  R () ->
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sep s f xs = sequence_ (intersperse s (f <$> xs))

-- | Render a collection of elements layout-sensitively using the given
-- printer, inserting semicolons if necessary and respecting the 'useBraces'
-- and 'dontUseBraces' combinators.
--
-- > useBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "{ foo; bar }") (txt "foo\nbar")
--
-- > dontUseBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "foo; bar") (txt "foo\nbar")
sepSemi ::
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sepSemi = sepSemi' False

-- | A version of 'sepSemi' that allows one to control whether semicolons
-- should be inserted in multi-line layout.
--
-- > useBraces $ sepSemi' False txt ["foo", "bar"]
-- >   == vlayout (txt "{ foo; bar }") (txt "foo\nbar")
--
-- > dontUseBraces $ sepSemi' True txt ["foo", "bar"]
-- >   == vlayout (txt "foo; bar") (txt "foo;\nbar")
sepSemi' ::
  -- | Whether to insert semicolons in multi-line layout
  Bool ->
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sepSemi' addMultiColSemi f xs = vlayout singleLine multiLine
  where
    singleLine = do
      ub <- canUseBraces
      case xs of
        [] -> when ub $ txt "{}"
        xs' ->
          if ub
            then do
              txt "{"
              space
              sep (txt ";" >> space) (dontUseBraces . f) xs'
              space
              txt "}"
            else sep (txt ";" >> space) f xs'
    multiLine =
      sep
        (if addMultiColSemi then txt ";" >> newline else newline)
        (dontUseBraces . f)
        xs

----------------------------------------------------------------------------
-- Wrapping

-- | 'BracketStyle' controlling how the closing bracket is rendered.
data BracketStyle
  = -- | Normal
    N
  | -- | Shifted one level
    S
  deriving (Eq, Show)

-- | Surround the given entity with backticks.
backticks :: R () -> R ()
backticks m = do
  txt "`"
  m
  txt "`"

-- | Surround the given entity with banana brackets (i.e. from arrow
-- notation).
banana :: BracketStyle -> R () -> R ()
banana = brackets_ True "(|" "|)"

-- | Surround the given entity with curly braces @{@ and @}@.
braces :: BracketStyle -> R () -> R ()
braces = brackets_ False "{" "}"

-- | Surround the given entity with square brackets @[@ and @]@.
brackets :: BracketStyle -> R () -> R ()
brackets = brackets_ False "[" "]"

-- | Surround the given entity with parentheses @(@ and @)@.
parens :: BracketStyle -> R () -> R ()
parens = brackets_ False "(" ")"

-- | Surround the given entity with @(# @ and @ #)@.
parensHash :: BracketStyle -> R () -> R ()
parensHash = brackets_ True "(#" "#)"

-- | Braces as used for pragmas: @{-#@ and @#-}@.
pragmaBraces :: R () -> R ()
pragmaBraces m = sitcc $ do
  txt "{-#"
  space
  m
  breakpoint
  inci (txt "#-}")

-- | Surround the body with a pragma name and 'pragmaBraces'.
pragma ::
  -- | Pragma text
  Text ->
  -- | Pragma body
  R () ->
  R ()
pragma pragmaText body = pragmaBraces $ do
  txt pragmaText
  breakpoint
  body

-- | A helper for defining wrappers like 'parens' and 'braces'.
brackets_ ::
  -- | Insert breakpoints around brackets
  Bool ->
  -- | Opening bracket
  Text ->
  -- | Closing bracket
  Text ->
  -- | Bracket style
  BracketStyle ->
  -- | Inner expression
  R () ->
  R ()
brackets_ needBreaks open close style m = sitcc (vlayout singleLine multiLine)
  where
    singleLine = do
      txt open
      when needBreaks space
      m
      when needBreaks space
      txt close
    multiLine = do
      txt open
      if needBreaks
        then newline >> inci m
        else space >> sitcc m
      newline
      inciIf (style == S) (txt close)

----------------------------------------------------------------------------
-- Literals

-- | Print @,@.
comma :: R ()
comma = txt ","

-- | Delimiting combination with 'comma'. To be used with 'sep'.
commaDel :: R ()
commaDel = comma >> breakpoint

----------------------------------------------------------------------------
-- Placement

-- | Expression placement. This marks the places where expressions that
-- support hanging forms may use them.
data Placement
  = -- | Multi-line layout should cause
    -- insertion of a newline and an
    -- indentation bump
    Normal
  | -- | Expressions that have a hanging form
    -- should use it and avoid bumping one level
    -- of indentation
    Hanging
  deriving (Eq, Show)

-- | Place a thing that may have a hanging form. This function handles how
-- to separate it from preceding expressions and whether to bump indentation
-- depending on what sort of expression we have.
placeHanging :: Placement -> R () -> R ()
placeHanging placement m =
  case placement of
    Hanging -> do
      space
      m
    Normal -> do
      breakpoint
      inci m

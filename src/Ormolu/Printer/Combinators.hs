{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Printing combinators. The definitions here are presented in such an
-- order so you can just go through the Haddocks and by the end of the file
-- you should have a pretty good idea how to program rendering logic.

module Ormolu.Printer.Combinators
  ( -- * The 'R' monad
    R
  , runR
  , getAnns
  , getEnclosingSpan
    -- * Combinators
    -- ** Basic
  , txt
  , atom
  , newline
  , inci
  , located
  , located'
  , switchLayout
  , vlayout
  , breakpoint
  , breakpoint'
    -- ** Formatting lists
  , sep
  , sepSemi
  , canUseBraces
  , useBraces
  , dontUseBraces
    -- ** Wrapping
  , sitcc
  , line
  , backticks
  , banana
  , braces
  , brackets
  , bracketsPar
  , parens
  , parensHash
  , pragmaBraces
  , pragma
    -- ** Literals
  , comma
  , space
  )
where

import Control.Monad
import Data.Data (Data)
import Data.List (intersperse)
import Data.Text (Text)
import Ormolu.Printer.Internal
import Ormolu.Utils (isModule, showOutputable)
import Outputable (Outputable)
import SrcLoc
import qualified Data.Text as T

----------------------------------------------------------------------------
-- Basic

-- | Output a fixed 'Text' fragment. The argument may not contain any line
-- breaks or tab characters. 'txt' is used to output all sorts of “fixed”
-- bits of syntax like keywords and pipes @|@ in functional dependencies.

txt :: Text -> R ()
txt t = ensureIndent >> spit t

-- | Output 'Outputable' fragment of AST. This can be used to output numeric
-- literals and similar. Everything that doesn't have inner structure but
-- does have an 'Outputable' instance.

atom :: Outputable a => a -> R ()
atom = txt . T.pack . showOutputable

-- | Enter a 'Located' entity. This combinator handles outputting comments
-- and sets layout (single-line vs multi-line) for the inner computation.
-- Roughly, the rule for using 'located' is that every time there is a
-- 'Located' wrapper, it should be “discharged” with a corresponding
-- 'located' invocation.

located
  :: Data a
  => Located a                  -- ^ Thing to enter
  -> (a -> R ())                -- ^ How to render inner value
  -> R ()
located loc f = do
  let sp = case getLoc loc of
             UnhelpfulSpan _ -> Nothing
             RealSrcSpan s -> Just s
  let setEnclosingSpan =
        if isModule (unLoc loc)
          then id
          else maybe id withEnclosingSpan sp
  start <- getPosition
  setEnclosingSpan $ switchLayout [getLoc loc] (f (unLoc loc))
  end <- getPosition
  mapM_ (\s -> updateSourceMap (s, mkRealSrcSpan start end)) sp

-- | A version of 'located' with arguments flipped.

located'
  :: Data a
  => (a -> R ())                -- ^ How to render inner value
  -> Located a                  -- ^ Thing to enter
  -> R ()
located' = flip located

-- | Set layout according to combination of given 'SrcSpan's for a given.
-- Use this only when you need to set layout based on e.g. combined span of
-- several elements when there is no corresponding 'Located' wrapper
-- provided by GHC AST. It is relatively rare that this one is needed.
--
-- Given empty list this function will set layout to single line.

switchLayout
  :: [SrcSpan]                  -- ^ Span that controls layout
  -> R ()                       -- ^ Computation to run with changed layout
  -> R ()
switchLayout spans' = enterLayout (spansLayout spans')

-- | Which layout combined spans result in?

spansLayout :: [SrcSpan] -> Layout
spansLayout = \case
  [] -> SingleLine
  (x:xs) ->
    if isOneLineSpan (foldr combineSrcSpans x xs)
      then SingleLine
      else MultiLine

-- | Insert a space if enclosing layout is single-line, or newline if it's
-- multiline.
--
-- > breakpoint = vlayout space newline

breakpoint :: R ()
breakpoint = vlayout space newline

-- | Similar to 'breakpoint' but outputs nothing in case of single-line
-- layout.
--
-- > breakpoint' = vlayout (return ()) newline

breakpoint' :: R ()
breakpoint' = vlayout (return ()) newline

----------------------------------------------------------------------------
-- Formatting lists

-- | Render a collection of elements inserting a separator between them.

sep
  :: R ()                       -- ^ Separator
  -> (a -> R ())                -- ^ How to render an element
  -> [a]                        -- ^ Elements to render
  -> R ()
sep s f xs = sequence_ (intersperse s (f <$> xs))

-- | Render a collection of elements layout-sensitively using given printer,
-- inserting semicolons if necessary and respecting 'useBraces' and
-- 'dontUseBraces' combinators.
--
-- > useBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "{ foo; bar }") (txt "foo\nbar")
--
-- > dontUseBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "foo; bar") (txt "foo\nbar")

sepSemi
  :: (a -> R ())                -- ^ How to render an element
  -> [a]                        -- ^ Elements to render
  -> R ()
sepSemi f xs = vlayout singleLine multiLine
  where
    singleLine = do
      ub <- canUseBraces
      case xs of
        [] -> when ub $ txt "{}"
        xs' ->
          if ub
          then do
            txt "{ "
            sep (txt "; ") (dontUseBraces . f) xs'
            txt " }"
          else
            sep (txt "; ") f xs'
    multiLine =
      sep newline (dontUseBraces . f) xs

----------------------------------------------------------------------------
-- Wrapping

-- | Finish given entity by a 'newline'.
--
-- > line m = do
-- >   m
-- >   newline

line :: R () -> R ()
line m = do
  m
  newline

-- | Surround given entity by backticks.

backticks :: R () -> R ()
backticks m = do
  txt "`"
  m
  txt "`"

-- | Surround given entity by banana brackets (i.e., from arrow notation.)

banana :: R () -> R ()
banana m = sitcc $ do
  txt "(|"
  ospaces m
  txt "|)"

-- | Surround given entity by curly braces @{@ and  @}@.

braces :: R () -> R ()
braces m = sitcc $ do
  txt "{"
  ospaces m
  txt "}"

-- | Surround given entity by square brackets @[@ and @]@.

brackets :: R () -> R ()
brackets m = sitcc $ do
  txt "["
  ospaces m
  txt "]"

-- | Surround given entity by parallel array brackets @[:@ and @:]@.

bracketsPar :: R () -> R ()
bracketsPar m = sitcc $ do
  txt "[: "
  m
  vlayout (return ()) space
  txt " :]"

-- | Surround given entity by parentheses @(@ and @)@.

parens :: R () -> R ()
parens m = sitcc $ do
  txt "("
  ospaces m
  txt ")"

-- | Surround given entity by @(# @ and @ #)@.

parensHash :: R () -> R ()
parensHash m = sitcc $ do
  txt "(# "
  m
  vlayout space (newline >> txt "  ")
  txt "#)"

-- | Braces as used for pragmas: @{-#@ and @#-}@.

pragmaBraces :: R () -> R ()
pragmaBraces m = sitcc $ do
  txt "{-# "
  m
  vlayout space (newline >> txt "  ")
  txt "#-}"

-- | Surround the body with a pragma name and 'pragmaBraces'.

pragma
  :: Text                       -- ^ Pragma text
  -> R ()                       -- ^ Pragma body
  -> R ()
pragma pragmaText body = pragmaBraces $ do
  txt pragmaText
  breakpoint
  body

-- | Surround given entity by optional space before and a newline after, iff
-- current layout is multiline.

ospaces :: R () -> R ()
ospaces m = vlayout m $ do
  space
  sitcc m
  newline
  txt "  "

----------------------------------------------------------------------------
-- Literals

-- | Print @,@.

comma :: R ()
comma = txt ","

-- | Print single space.

space :: R ()
space = txt " "

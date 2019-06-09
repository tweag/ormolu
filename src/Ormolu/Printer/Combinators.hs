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
    -- * Combinators
    -- ** Basic
  , txt
  , atom
  , newline
  , inci
  , located
  , locatedVia
  , located'
  , switchLayout
  , vlayout
  , breakpoint
    -- ** Formatting lists
  , velt
  , velt'
  , withSep
  , spaceSep
  , newlineSep
    -- ** Wrapping
  , sitcc
  , line
  , backticks
  , braces
  , brackets
  , bracketsPar
  , parens
  , parensHash
  , pragmaBraces
    -- ** Literals
  , comma
  , space
  )
where

import Data.Data (Data)
import Data.List (intersperse)
import Data.Text (Text)
import Ormolu.Printer.Comments
import Ormolu.Printer.Internal
import Ormolu.Utils (unL, getSpan, isModule)
import Outputable (Outputable (..), showSDocUnsafe)
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
atom = txt . T.pack . showSDocUnsafe . ppr

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
located loc@(L l _) = locatedVia (Just l) loc

-- | A special version of 'located' that allows to control layout using an
-- externally provided span. 'Nothing' means that layout won't be changed.

locatedVia
  :: Data a
  => Maybe SrcSpan              -- ^ Span that controls layout selection
  -> Located a                  -- ^ Thing to enter
  -> (a -> R ())                -- ^ How to render inner value
  -> R ()
locatedVia ml loc f = do
  let withRealLocated (L l a) g =
        case l of
          UnhelpfulSpan _ -> return ()
          RealSrcSpan l' -> g (L l' a)
  withRealLocated loc spitPrecedingComments
  let setEnclosingSpan =
        case getSpan loc of
          UnhelpfulSpan _ -> id
          RealSrcSpan orf ->
            if isModule (unL loc)
              then id
              else withEnclosingSpan orf
  setEnclosingSpan $ case ml of
     Nothing -> f (unL loc)
     Just l' -> switchLayout l' (f (unL loc))
  withRealLocated loc spitFollowingComments

-- | A version of 'located' with arguments flipped.

located'
  :: Data a
  => (a -> R ())                -- ^ How to render inner value
  -> Located a                  -- ^ Thing to enter
  -> R ()
located' = flip located

-- | Set layout according to given 'SrcSpan' for a given computation. Use
-- this only when you need to set layout based on e.g. combined span of
-- several elements when there is no corresponding 'Located' wrapper
-- provided by GHC AST. It is relatively rare that this one is needed.

switchLayout
  :: SrcSpan                    -- ^ Span that controls layout
  -> R ()                       -- ^ Computation to run with changed layout
  -> R ()
switchLayout spn = enterLayout
  (if isOneLineSpan spn
    then SingleLine
    else MultiLine)

-- | Insert a space if enclosing layout is single-line, or newline if it's
-- multiline.
--
-- > breakpoint = vlayout space newline

breakpoint :: R ()
breakpoint = vlayout space newline

----------------------------------------------------------------------------
-- Formatting lists

-- | Element of variable layout. This means that the sub-components may be
-- rendered either on single line or each on its own line depending on
-- current layout.
--
-- This version does not make subsequent element (second and later) align
-- with the first automatically and does not insert spaces between elements
-- when layout is single line.

velt :: [R ()] -> R ()
velt xs = sequence_ (intersperse sep (sitcc <$> xs))
  where
    sep = vlayout (pure ()) newline

-- | Like 'velt', but all sub-elements start at the same indentation level
-- as first element, additionally spaces are inserted when layout is single
-- line.

velt' :: [R ()] -> R ()
velt' xs = sitcc $ sequence_ (intersperse breakpoint (sitcc <$> xs))

-- | Put separator between renderings of items of a list.

withSep
  :: R ()                       -- ^ Separator
  -> (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> [R ()]                     -- ^ List of printing actions
withSep sep f = \case
  [] -> []
  (x:xs) ->
    let g a = sep >> f a
    in f x : fmap g xs

-- | Render space-separated elements.
--
-- > spaceSep f = sequence_ . withSep space f

spaceSep
  :: (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> R ()
spaceSep f = sequence_ . withSep space f

-- | Render newline-separated elements.
--
-- > newlineSep f = sequence_ . withSep newline f

newlineSep
  :: (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> R ()
newlineSep f = sequence_ . withSep newline f

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
  breakpoint
  txt "#)"

-- | Braces as used for pragmas: @{-#@ and @#-}@.

pragmaBraces :: R () -> R ()
pragmaBraces m = sitcc $ do
  txt "{-# "
  m
  vlayout space (newline >> txt "  ")
  txt "#-}"

-- | Surround given entity by optional space before and a newline after, iff
-- current layout is multiline.

ospaces :: R () -> R ()
ospaces m = vlayout m (txt " " >> m >> newline)

----------------------------------------------------------------------------
-- Literals

-- | Print @,@ followed by a space.

comma :: R ()
comma = txt ", "

-- | Print single space.

space :: R ()
space = txt " "

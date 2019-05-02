{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Printing combinators.

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
  , relaxComments
  , hasMoreComments
  , located
  , locatedVia
  , located'
  , switchLayout
  , velt
  , velt'
  , vlayout
  , breakpoint
  , withSep
  , spaceSep
  , newlineSep
    -- ** Wrapping
  , line
  , backticks
  , braces
  , brackets
  , bracketsPar
  , parens
  , parensHash
    -- ** Literals
  , comma
  , space
  )
where

import Data.Bool (bool)
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
-- breaks or tab characters.

txt :: Text -> R ()
txt t = ensureIndent >> spit t

-- | Output 'Outputable' fragment of AST.

atom :: Outputable a => a -> R ()
atom = txt . T.pack . showSDocUnsafe . ppr

-- | Enter a 'Located' entity. This primitive handles outputting comments
-- that may be associated with the primitive and sets corresponding layout
-- for the inner computation.

located
  :: Data a
  => Located a                  -- ^ Thing to enter
  -> (a -> R ())                -- ^ How to render inner value
  -> R ()
located loc@(L l _) = locatedVia (Just l) loc

-- | A special version of 'located' that allows to control layout using
-- externally provided span. 'Nothing' means that layout won't be changed.

locatedVia
  :: Data a
  => Maybe SrcSpan              -- ^ Span that controls layout selection
  -> Located a                  -- ^ Thing to enter
  -> (a -> R ())                -- ^ How to render inner value
  -> R ()
locatedVia ml loc f = do
  relaxed <- relaxedComments
  bool sitcc id relaxed $ do
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
-- provided by GHC AST.

switchLayout
  :: SrcSpan                    -- ^ Span that controls layout
  -> R ()                       -- ^ Computation to run with changed layout
  -> R ()
switchLayout spn = enterLayout
  (if isOneLineSpan spn
    then SingleLine
    else MultiLine)

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
velt' xs = sitcc $ sequence_ (intersperse sep (sitcc <$> xs))
  where
    sep = vlayout (spit " ") newline

-- | Insert a space if enclosing layout is single-line, or newline if it's
-- multiline.

breakpoint :: R ()
breakpoint = vlayout space newline

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

spaceSep
  :: (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> R ()
spaceSep f = sequence_ . withSep space f

-- | Render newline-separated elements.

newlineSep
  :: (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> R ()
newlineSep f = sequence_ . withSep newline f

----------------------------------------------------------------------------
-- Wrapping

-- | Finish given entity by a 'newline'.

line :: R () -> R ()
line m = do
  m
  newline

-- | Surround given entity by backticks.

backticks :: R () -> R ()
backticks m = txt "`" >> m >> txt "`"

-- | Surround given entity by curly braces.

braces :: R () -> R ()
braces m = sitcc $ do
  txt "{"
  ospaces m
  txt "}"

-- | Surround given entity by square brackets.

brackets :: R () -> R ()
brackets m = sitcc $ do
  txt "["
  ospaces m
  txt "]"

-- | Surround given entity by parallel array brackets @[:@ ond @:]@.

bracketsPar :: R () -> R ()
bracketsPar m = sitcc $ do
  txt "[:"
  m
  txt ":]"

-- | Surround given entity by parentheses.

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
  txt " #)"

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

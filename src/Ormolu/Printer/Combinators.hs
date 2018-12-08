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
  , located
  , velt
  , velt'
    -- ** Wrapping
  , line
  , braces
  , brackets
  , parens
    -- ** Literals
  , comma
  , ofType
  , sarrow
  , darrow
  )
where

import Data.Data (Data)
import Data.List (intersperse)
import Data.Text (Text)
import Debug.Trace
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Internal
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

located :: Data a => Located a -> (a -> R ()) -> R ()
located loc@(L l a) f = do
  -- TODO implement handling of comments properly
  mann <- lookupAnn loc
  let m = enterLayout
        (if isOneLineSpan l
          then SingleLine
          else MultiLine)
        (f a)
  case mann of
    Nothing -> m
    Just Ann {..} ->
      enterLayout MultiLine $ do
        traceShow annPriorComments $ mapM_ atom annPriorComments
        m
        traceShow annFollowingComments $ mapM_ atom annFollowingComments

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

----------------------------------------------------------------------------
-- Wrapping

-- | Finish given entity by a 'newline'.

line :: R () -> R ()
line m = do
  m
  newline

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

-- | Surround given entity by parentheses.

parens :: R () -> R ()
parens m = sitcc $ do
  txt "("
  ospaces m
  txt ")"

-- | Surround given entity by optional space before and a newline after, iff
-- current layout is multiline.

ospaces :: R () -> R ()
ospaces m = vlayout m (txt " " >> m >> newline)

----------------------------------------------------------------------------
-- Literals

-- | Print @,@ followed by a space.

comma :: R ()
comma = txt ", "

-- | Print @::@ followed by a space.

ofType :: R ()
ofType = txt ":: "

-- | Print @->@ followed by a space.

sarrow :: R ()
sarrow = txt "-> "

-- | Print @=>@ followed by a space.

darrow :: R ()
darrow = txt "=> "

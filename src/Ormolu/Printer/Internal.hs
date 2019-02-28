{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
  , ensureIndent
  , inci
  , sitcc
  , Layout (..)
  , enterLayout
  , vlayout
  , currentLayout
  , lookupAnn
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Data (Data)
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Debug.Trace
import Language.Haskell.GHC.ExactPrint.Types
import SrcLoc
import qualified Data.Text.Lazy as TL
import qualified Data.Map  as M
import qualified Data.Text as T

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
  , rcAnns :: Anns
    -- ^ The collection of annotations obtained after parsing
  , rcDebug :: Bool
    -- ^ Whether to print debugging info as we go
  }

-- | State context of 'R'.

data SC = SC
  { scColumn :: !Int
    -- ^ Index of the next column to render
  , scBuilder :: Builder
    -- ^ Rendered source code so far
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
  -> Anns                       -- ^ Annotations to use
  -> Text                       -- ^ Resulting rendition
runR debug (R m) anns =
  TL.toStrict . toLazyText . scBuilder $ execState (runReaderT m rc) sc
  where
    rc = RC
      { rcIndent = 0
      , rcLayout = MultiLine
      , rcAnns = anns
      , rcDebug = debug
      }
    sc = SC
      { scColumn = 0
      , scBuilder = mempty
      }

----------------------------------------------------------------------------
-- Internal functions

-- | Grow current output by appending given 'Text' to it, which may not
-- contain newlines.

spit :: Text -> R ()
spit x = do
  traceR "spit_before" (Just x)
  R (modify modSC)
  traceR "spit_after" Nothing
  where
    modSC sc = sc
      { scBuilder = scBuilder sc <> fromText x
      , scColumn = scColumn sc + T.length x
      }

-- | Output a newline.

newline :: R ()
newline = do
  traceR "newline_before" (Just "\n")
  R (modify modSC)
  traceR "newline_after" Nothing
  where
    modSC sc = sc
      { scBuilder = scBuilder sc <> "\n"
      , scColumn = 0
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
-- computation.

inci :: R () -> R ()
inci m' = do
  traceR "inci_before" Nothing
  let R m = traceR "inci_inside" Nothing >> m'
  R (local modRC m)
  traceR "inci_ended" Nothing
  where
    modRC x = x
      { rcIndent = rcIndent x + indentStep
      }

-- | Set indentation level for the inner computation equal to current
-- column. This makes sure that the entire inner block is uniformly
-- \"shifted\" to the right. Only works (and makes sense) when enclosing
-- layout is multilined.

sitcc :: R () -> R ()
sitcc m' = do
  traceR "sitcc_before" Nothing
  i <- R (asks rcIndent)
  c <- R (gets scColumn)
  let modRC x = x
        { rcIndent = max i c
        }
      R m = traceR "sitcc_inside" Nothing >> m'
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
  let modRC x = x
        { rcLayout = l
        }
  R (local modRC m)
  traceR ("lend_" ++ label) Nothing

-- | Do one or another thing depending on current 'Layout'.

vlayout
  :: R ()                       -- ^ Single line
  -> R ()                       -- ^ Multi line
  -> R ()
vlayout sline mline = do
  l <- currentLayout
  case l of
    SingleLine -> sline
    MultiLine -> mline

-- | Return current layout.

currentLayout :: R Layout
currentLayout = R (asks rcLayout)

-- | Lookup an annotation.

lookupAnn :: Data a => Located a -> R (Maybe Annotation)
lookupAnn l = M.lookup (mkAnnKey l) <$> R (asks rcAnns)

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

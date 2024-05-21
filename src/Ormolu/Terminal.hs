{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An abstraction for colorful output in terminal.
module Ormolu.Terminal
  ( -- * The 'Term' abstraction
    Term,
    ColorMode (..),
    runTerm,
    runTermPure,

    -- * Styling
    bold,
    cyan,
    green,
    red,

    -- * Printing
    put,
    putShow,
    putOutputable,
    newline,
  )
where

import Control.Applicative (Const (..))
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T.Utf8
import GHC.Utils.Outputable (Outputable)
import Ormolu.Utils (showOutputable)
import System.Console.ANSI
import System.IO (Handle, hFlush)

----------------------------------------------------------------------------
-- The 'Term' abstraction

type Term = TermOutput ()

newtype TermOutput a = TermOutput (Const (Seq TermOutputNode) a)
  deriving (Semigroup, Monoid, Functor, Applicative)

data TermOutputNode
  = OutputText Text
  | WithColor Color Term
  | WithBold Term

singleTerm :: TermOutputNode -> Term
singleTerm = TermOutput . Const . Seq.singleton

-- | Whether to use colors and other features of ANSI terminals.
data ColorMode = Never | Always | Auto
  deriving (Eq, Show)

-- | Run 'Term' monad.
runTerm ::
  Term ->
  -- | Color mode
  ColorMode ->
  -- | Handle to print to
  Handle ->
  IO ()
runTerm term0 colorMode handle = do
  useSGR <- case colorMode of
    Never -> return False
    Always -> return True
    Auto -> hSupportsANSI handle
  runTerm' useSGR term0
  hFlush handle
  where
    runTerm' useSGR = go
      where
        go (TermOutput (Const nodes)) =
          forM_ nodes $ \case
            OutputText s -> T.Utf8.hPutStr handle s
            WithColor color term -> withSGR [SetColor Foreground Dull color] (go term)
            WithBold term -> withSGR [SetConsoleIntensity BoldIntensity] (go term)

        withSGR sgrs m
          | useSGR = hSetSGR handle sgrs >> m >> hSetSGR handle [Reset]
          | otherwise = m

runTermPure :: Term -> Text
runTermPure (TermOutput (Const nodes)) =
  T.concat . toList . flip fmap nodes $ \case
    OutputText s -> s
    WithColor _ term -> runTermPure term
    WithBold term -> runTermPure term

----------------------------------------------------------------------------
-- Styling

-- | Make the output bold text.
bold :: Term -> Term
bold = singleTerm . WithBold

-- | Make the output cyan text.
cyan :: Term -> Term
cyan = singleTerm . WithColor Cyan

-- | Make the output green text.
green :: Term -> Term
green = singleTerm . WithColor Green

-- | Make the output red text.
red :: Term -> Term
red = singleTerm . WithColor Red

----------------------------------------------------------------------------
-- Printing

-- | Output 'Text'.
put :: Text -> Term
put = singleTerm . OutputText

-- | Output a 'Show' value.
putShow :: (Show a) => a -> Term
putShow = put . T.pack . show

-- | Output an 'Outputable' value.
putOutputable :: (Outputable a) => a -> Term
putOutputable = put . T.pack . showOutputable

-- | Output a newline.
newline :: Term
newline = put "\n"

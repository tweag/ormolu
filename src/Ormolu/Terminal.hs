{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | An abstraction for colorful output in terminal.
module Ormolu.Terminal
  ( -- * The 'Term' monad
    Term,
    ColorMode (..),
    runTerm,

    -- * Styling
    bold,
    cyan,
    green,
    red,

    -- * Printing
    put,
    putS,
    putSrcSpan,
    newline,
  )
where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Types.SrcLoc
import Ormolu.Utils (showOutputable)
import System.Console.ANSI
import System.IO (Handle, hFlush, hPutStr)

----------------------------------------------------------------------------
-- The 'Term' monad

-- | Terminal monad.
newtype Term a = Term (ReaderT RC IO a)
  deriving (Functor, Applicative, Monad)

-- | Reader context of 'Term'.
data RC = RC
  { -- | Whether to use colors
    rcUseColor :: Bool,
    -- | Handle to print to
    rcHandle :: Handle
  }

-- | Whether to use colors and other features of ANSI terminals.
data ColorMode = Never | Always | Auto
  deriving (Eq, Show)

-- | Run 'Term' monad.
runTerm ::
  -- | Monad to run
  Term a ->
  -- | Color mode
  ColorMode ->
  -- | Handle to print to
  Handle ->
  IO a
runTerm (Term m) colorMode rcHandle = do
  rcUseColor <- case colorMode of
    Never -> return False
    Always -> return True
    Auto -> hSupportsANSI rcHandle
  x <- runReaderT m RC {..}
  hFlush rcHandle
  return x

----------------------------------------------------------------------------
-- Styling

-- | Make the inner computation output bold text.
bold :: Term a -> Term a
bold = withSGR [SetConsoleIntensity BoldIntensity]

-- | Make the inner computation output cyan text.
cyan :: Term a -> Term a
cyan = withSGR [SetColor Foreground Dull Cyan]

-- | Make the inner computation output green text.
green :: Term a -> Term a
green = withSGR [SetColor Foreground Dull Green]

-- | Make the inner computation output red text.
red :: Term a -> Term a
red = withSGR [SetColor Foreground Dull Red]

-- | Alter 'SGR' for inner computation.
withSGR :: [SGR] -> Term a -> Term a
withSGR sgrs (Term m) = Term $ do
  RC {..} <- ask
  if rcUseColor
    then do
      liftIO $ hSetSGR rcHandle sgrs
      x <- m
      liftIO $ hSetSGR rcHandle [Reset]
      return x
    else m

----------------------------------------------------------------------------
-- Printing

-- | Output 'Text'.
put :: Text -> Term ()
put txt = Term $ do
  RC {..} <- ask
  liftIO $ T.hPutStr rcHandle txt

-- | Output 'String'.
putS :: String -> Term ()
putS str = Term $ do
  RC {..} <- ask
  liftIO $ hPutStr rcHandle str

-- | Output a 'GHC.SrcSpan'.
putSrcSpan :: SrcSpan -> Term ()
putSrcSpan = putS . showOutputable

-- | Output a newline.
newline :: Term ()
newline = Term $ do
  RC {..} <- ask
  liftIO $ T.hPutStr rcHandle "\n"

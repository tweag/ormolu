{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | 'OrmoluException' type and surrounding definitions.
module Ormolu.Exception
  ( OrmoluException (..),
    withPrettyOrmoluExceptions,
  )
where

import Control.Exception
import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified GHC
import Ormolu.Diff.Text (TextDiff, printTextDiff)
import Ormolu.Terminal
import System.Exit (ExitCode (..))
import System.IO

-- | Ormolu exception representing all cases when Ormolu can fail.
data OrmoluException
  = -- | Parsing of original source code failed
    OrmoluParsingFailed GHC.SrcSpan String
  | -- | Parsing of formatted source code failed
    OrmoluOutputParsingFailed GHC.SrcSpan String
  | -- | Original and resulting ASTs differ
    OrmoluASTDiffers FilePath [GHC.SrcSpan]
  | -- | Formatted source code is not idempotent
    OrmoluNonIdempotentOutput TextDiff
  | -- | Some GHC options were not recognized
    OrmoluUnrecognizedOpts (NonEmpty String)
  deriving (Eq, Show)

instance Exception OrmoluException

-- | Print an 'OrmoluException'.
printOrmoluException ::
  OrmoluException ->
  Term ()
printOrmoluException = \case
  OrmoluParsingFailed s e -> do
    bold (putSrcSpan s)
    newline
    put "  The GHC parser (in Haddock mode) failed:"
    newline
    put "  "
    put (T.pack e)
    newline
  OrmoluOutputParsingFailed s e -> do
    bold (putSrcSpan s)
    newline
    put "  Parsing of formatted code failed:"
    put "  "
    put (T.pack e)
    newline
  OrmoluASTDiffers path ss -> do
    putS path
    newline
    put "  AST of input and AST of formatted code differ."
    newline
    forM_ ss $ \s -> do
      put "    at "
      putSrcSpan s
      newline
    put "  Please, consider reporting the bug."
    newline
  OrmoluNonIdempotentOutput diff -> do
    printTextDiff diff
    newline
    put "  Formatting is not idempotent."
    newline
    put "  Please, consider reporting the bug."
    newline
  OrmoluUnrecognizedOpts opts -> do
    put "The following GHC options were not recognized:"
    newline
    put "  "
    (putS . unwords . NE.toList) opts
    newline

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely.
withPrettyOrmoluExceptions ::
  -- | Color mode
  ColorMode ->
  -- | Action that may throw an exception
  IO ExitCode ->
  IO ExitCode
withPrettyOrmoluExceptions colorMode m = m `catch` h
  where
    h e = do
      runTerm (printOrmoluException e) colorMode stderr
      return . ExitFailure $
        case e of
          -- Error code 1 is for 'error' or 'notImplemented'
          -- 2 used to be for erroring out on CPP
          OrmoluParsingFailed {} -> 3
          OrmoluOutputParsingFailed {} -> 4
          OrmoluASTDiffers {} -> 5
          OrmoluNonIdempotentOutput {} -> 6
          OrmoluUnrecognizedOpts {} -> 7

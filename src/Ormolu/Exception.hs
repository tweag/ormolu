{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Types.SrcLoc
import Ormolu.Diff.Text (TextDiff, printTextDiff)
import Ormolu.Terminal
import System.Exit (ExitCode (..))
import System.IO
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

-- | Ormolu exception representing all cases when Ormolu can fail.
data OrmoluException
  = -- | Parsing of original source code failed
    OrmoluParsingFailed SrcSpan String
  | -- | Parsing of formatted source code failed
    OrmoluOutputParsingFailed SrcSpan String
  | -- | Original and resulting ASTs differ
    OrmoluASTDiffers TextDiff [RealSrcSpan]
  | -- | Formatted source code is not idempotent
    OrmoluNonIdempotentOutput TextDiff
  | -- | Some GHC options were not recognized
    OrmoluUnrecognizedOpts (NonEmpty String)
  | -- | Cabal file parsing failed
    OrmoluCabalFileParsingFailed FilePath
  | -- | Missing input file path when using stdin input and
    -- accounting for .cabal files
    OrmoluMissingStdinInputFile
  | -- | A parse error in a fixity overrides file
    OrmoluFixityOverridesParseError (ParseErrorBundle Text Void)
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
  OrmoluASTDiffers diff ss -> do
    printTextDiff diff
    newline
    put "  AST of input and AST of formatted code differ."
    newline
    forM_ ss $ \s -> do
      put "    at "
      putRealSrcSpan s
      newline
    put "  Please, consider reporting the bug."
    newline
    put "  To format anyway, use --unsafe."
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
  OrmoluCabalFileParsingFailed cabalFile -> do
    put "Parsing this .cabal file failed:"
    newline
    put $ "  " <> T.pack cabalFile
    newline
  OrmoluMissingStdinInputFile -> do
    put "The --stdin-input-file option is necessary when using input"
    newline
    put "from stdin and accounting for .cabal files"
    newline
  OrmoluFixityOverridesParseError errorBundle -> do
    putS (errorBundlePretty errorBundle)
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
          OrmoluCabalFileParsingFailed {} -> 8
          OrmoluMissingStdinInputFile {} -> 9
          OrmoluFixityOverridesParseError {} -> 10

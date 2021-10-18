{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad
import Data.Bool (bool)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative
import Ormolu
import Ormolu.Diff.Text (diffText, printTextDiff)
import Ormolu.Parser (manualExts)
import Ormolu.Terminal
import Ormolu.Utils (showOutputable)
import Ormolu.Utils.Extensions (getCabalExtensionDynOptions)
import Ormolu.Utils.IO
import Paths_ormolu (version)
import System.Exit (ExitCode (..), exitWith)
import qualified System.FilePath as FP
import System.IO (hPutStrLn, stderr)

-- | Entry point of the program.
main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  let formatOne' =
        formatOne
          optCabalDefaultExtensions
          optMode
          optSourceType
          optConfig
  exitCode <- case optInputFiles of
    [] -> formatOne' Nothing
    ["-"] -> formatOne' Nothing
    [x] -> formatOne' (Just x)
    xs -> do
      let selectFailure = \case
            ExitSuccess -> Nothing
            ExitFailure n -> Just n
      errorCodes <-
        mapMaybe selectFailure <$> mapM (formatOne' . Just) (sort xs)
      return $
        if null errorCodes
          then ExitSuccess
          else
            ExitFailure $
              if all (== 100) errorCodes
                then 100
                else 102
  exitWith exitCode

-- | Format a single input.
formatOne ::
  -- | Whether to respect default-extensions from .cabal files
  CabalDefaultExtensionsOpts ->
  -- | Mode of operation
  Mode ->
  -- | The 'SourceType' requested by the user
  Maybe SourceType ->
  -- | Configuration
  Config RegionIndices ->
  -- | File to format or stdin as 'Nothing'
  Maybe FilePath ->
  IO ExitCode
formatOne CabalDefaultExtensionsOpts {..} mode reqSourceType rawConfig mpath =
  withPrettyOrmoluExceptions (cfgColorMode rawConfig) $ do
    case FP.normalise <$> mpath of
      -- input source = STDIN
      Nothing -> do
        resultConfig <-
          patchConfig Nothing
            <$> if optUseCabalDefaultExtensions
              then case optStdinInputFile of
                Just stdinInputFile ->
                  getCabalExtensionDynOptions stdinInputFile
                Nothing -> throwIO OrmoluMissingStdinInputFile
              else pure []
        case mode of
          Stdout -> do
            ormoluStdin resultConfig >>= TIO.putStr
            return ExitSuccess
          InPlace -> do
            hPutStrLn
              stderr
              "In place editing is not supported when input comes from stdin."
            -- 101 is different from all the other exit codes we already use.
            return (ExitFailure 101)
          Check -> do
            -- ormoluStdin is not used because we need the originalInput
            originalInput <- getContentsUtf8
            let stdinRepr = "<stdin>"
            formattedInput <-
              ormolu resultConfig stdinRepr (T.unpack originalInput)
            handleDiff originalInput formattedInput stdinRepr
      -- input source = a file
      Just inputFile -> do
        resultConfig <-
          patchConfig (Just (detectSourceType inputFile))
            <$> if optUseCabalDefaultExtensions
              then getCabalExtensionDynOptions inputFile
              else pure []
        case mode of
          Stdout -> do
            ormoluFile resultConfig inputFile >>= TIO.putStr
            return ExitSuccess
          InPlace -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- readFileUtf8 inputFile
            formattedInput <-
              ormolu resultConfig inputFile (T.unpack originalInput)
            when (formattedInput /= originalInput) $
              writeFileUtf8 inputFile formattedInput
            return ExitSuccess
          Check -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- readFileUtf8 inputFile
            formattedInput <-
              ormolu resultConfig inputFile (T.unpack originalInput)
            handleDiff originalInput formattedInput inputFile
  where
    patchConfig mdetectedSourceType dynOpts =
      rawConfig
        { cfgDynOptions = cfgDynOptions rawConfig ++ dynOpts,
          cfgSourceType =
            fromMaybe
              ModuleSource
              (reqSourceType <|> mdetectedSourceType)
        }
    handleDiff originalInput formattedInput fileRepr =
      case diffText originalInput formattedInput fileRepr of
        Nothing -> return ExitSuccess
        Just diff -> do
          runTerm (printTextDiff diff) (cfgColorMode rawConfig) stderr
          -- 100 is different to all the other exit code that are emitted
          -- either from an 'OrmoluException' or from 'error' and
          -- 'notImplemented'.
          return (ExitFailure 100)

----------------------------------------------------------------------------
-- Command line options parsing

data Opts = Opts
  { -- | Mode of operation
    optMode :: !Mode,
    -- | Ormolu 'Config'
    optConfig :: !(Config RegionIndices),
    -- | Options for respecting default-extensions from .cabal files
    optCabalDefaultExtensions :: CabalDefaultExtensionsOpts,
    -- | Source type option, where 'Nothing' means autodetection
    optSourceType :: !(Maybe SourceType),
    -- | Haskell source files to format or stdin (when the list is empty)
    optInputFiles :: ![FilePath]
  }

-- | Mode of operation.
data Mode
  = -- | Output formatted source code to stdout
    Stdout
  | -- | Overwrite original file
    InPlace
  | -- | Exit with non-zero status code if
    -- source is not already formatted
    Check
  deriving (Eq, Show)

-- | Configuration for how to account for default-extension
-- from .cabal files
data CabalDefaultExtensionsOpts = CabalDefaultExtensionsOpts
  { -- | Account for default-extensions from .cabal files
    optUseCabalDefaultExtensions :: Bool,
    -- | Optional path to a file which will be used to
    -- find a .cabal file when using input from stdin
    optStdinInputFile :: Maybe FilePath
  }
  deriving (Show)

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> exts <*> optsParser) . mconcat $
    [fullDesc]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      intercalate
        "\n"
        [ unwords
            [ "ormolu",
              showVersion version,
              $gitBranch,
              $gitHash
            ],
          "using ghc-lib-parser " ++ VERSION_ghc_lib_parser
        ]
    exts :: Parser (a -> a)
    exts =
      infoOption displayExts . mconcat $
        [ long "manual-exts",
          help "Display extensions that need to be enabled manually"
        ]
    displayExts = unlines $ sort (showOutputable <$> manualExts)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> ( (fmap (bool Stdout InPlace) . switch . mconcat)
            [ short 'i',
              help "A shortcut for --mode inplace"
            ]
            <|> (option parseMode . mconcat)
              [ long "mode",
                short 'm',
                metavar "MODE",
                value Stdout,
                help "Mode of operation: 'stdout' (the default), 'inplace', or 'check'"
              ]
        )
    <*> configParser
    <*> cabalDefaultExtensionsParser
    <*> sourceTypeParser
    <*> (many . strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source files to format or stdin (the default)"
      ]

cabalDefaultExtensionsParser :: Parser CabalDefaultExtensionsOpts
cabalDefaultExtensionsParser =
  CabalDefaultExtensionsOpts
    <$> (switch . mconcat)
      [ short 'e',
        long "cabal-default-extensions",
        help "Account for default-extensions from .cabal files"
      ]
    <*> (optional . strOption . mconcat)
      [ long "stdin-input-file",
        help "Path which will be used to find the .cabal file when using input from stdin"
      ]

configParser :: Parser (Config RegionIndices)
configParser =
  Config
    <$> (fmap (fmap DynOption) . many . strOption . mconcat)
      [ long "ghc-opt",
        short 'o',
        metavar "OPT",
        help "GHC options to enable (e.g. language extensions)"
      ]
    <*> (switch . mconcat)
      [ long "unsafe",
        short 'u',
        help "Do formatting faster but without automatic detection of defects"
      ]
    <*> (switch . mconcat)
      [ long "debug",
        short 'd',
        help "Output information useful for debugging"
      ]
    <*> (switch . mconcat)
      [ long "check-idempotence",
        short 'c',
        help "Fail if formatting is not idempotent"
      ]
    -- We cannot parse the source type here, because we might need to do
    -- autodection based on the input file extension (not available here)
    -- before storing the resolved value in the config struct.
    <*> pure ModuleSource
    <*> (option parseColorMode . mconcat)
      [ long "color",
        metavar "WHEN",
        value Auto,
        help "Colorize the output; WHEN can be 'never', 'always', or 'auto' (the default)"
      ]
    <*> ( RegionIndices
            <$> (optional . option auto . mconcat)
              [ long "start-line",
                metavar "START",
                help "Start line of the region to format (starts from 1)"
              ]
            <*> (optional . option auto . mconcat)
              [ long "end-line",
                metavar "END",
                help "End line of the region to format (inclusive)"
              ]
        )

sourceTypeParser :: Parser (Maybe SourceType)
sourceTypeParser =
  (option parseSourceType . mconcat)
    [ long "source-type",
      short 't',
      metavar "TYPE",
      value Nothing,
      help "Set the type of source; TYPE can be 'module', 'sig', or 'auto' (the default)"
    ]

----------------------------------------------------------------------------
-- Helpers

-- | Parse 'Mode'.
parseMode :: ReadM Mode
parseMode = eitherReader $ \case
  "stdout" -> Right Stdout
  "inplace" -> Right InPlace
  "check" -> Right Check
  s -> Left $ "unknown mode: " ++ s

-- | Parse 'ColorMode'.
parseColorMode :: ReadM ColorMode
parseColorMode = eitherReader $ \case
  "never" -> Right Never
  "always" -> Right Always
  "auto" -> Right Auto
  s -> Left $ "unknown color mode: " ++ s

-- | Parse the 'SourceType'. 'Nothing' means that autodetection based on
-- file extension is requested.
parseSourceType :: ReadM (Maybe SourceType)
parseSourceType = eitherReader $ \case
  "module" -> Right (Just ModuleSource)
  "sig" -> Right (Just SignatureSource)
  "auto" -> Right Nothing
  s -> Left $ "unknown source type: " ++ s

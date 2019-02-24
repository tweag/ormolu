{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Data.List (intercalate)
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative
import Ormolu
import Paths_ormolu (version)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml

-- | Entry point of the program.

main :: IO ()
main = withPrettyOrmoluExceptions $ do
  Opts {..} <- execParser optsParserInfo
  config <- case optConfigFile of
    Nothing -> return Config
      { cfgDynOptions = optDynOptions
      , cfgUnsafe = optUnsafe
      }
    Just path -> do
      Config {..} <- Yaml.decodeFileThrow path
      return Config
        { cfgDynOptions = cfgDynOptions ++ optDynOptions
        , cfgUnsafe = optUnsafe || cfgUnsafe
        }
  r <- ormoluFile config optDebug optInputFile
  case optMode of
    Stdout ->
      TIO.putStr r
    InPlace ->
      TIO.writeFile optInputFile r
    Check -> do
      r' <- TIO.readFile optInputFile
      when (r /= r') exitFailure

----------------------------------------------------------------------------
-- Command line options parsing.

data Opts = Opts
  { optMode :: !Mode
    -- ^ Mode of operation
  , optConfigFile :: !(Maybe FilePath)
    -- ^ Location of configuration file (optional)
  , optUnsafe :: !Bool
    -- ^ Whether to skip sanity checking
  , optDynOptions :: ![DynOption]
    -- ^ GHC options to set
  , optDebug :: !Bool
    -- ^ Output information useful for debugging
  , optInputFile :: !FilePath
    -- ^ Input source file
  }

-- | Mode of operation.

data Mode
  = Stdout                      -- ^ Output formatted source code to stdout
  | InPlace                     -- ^ Overwrite original file
  | Check                       -- ^ Exit with non-zero status code if
                                -- source is not already formatted
  deriving (Eq, Show)

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (helper <*> ver <*> optsParser) . mconcat $
  [ fullDesc
  , progDesc ""
  , header ""
  ]
  where
    ver :: Parser (a -> a)
    ver = infoOption verStr . mconcat $
      [ long "version"
      , short 'v'
      , help "Print version of the program"
      ]
    verStr = intercalate "\n"
      [ unwords
        [ "ormolu"
        , showVersion version
        , $gitBranch
        , $gitHash
        ]
      , "using ghc-exactprint " ++ VERSION_ghc_exactprint
      , "using ghc            " ++ VERSION_ghc
      ]

optsParser :: Parser Opts
optsParser = Opts
  <$> (option parseMode . mconcat)
    [ long "mode"
    , short 'm'
    , metavar "MODE"
    , value Stdout
    , help "Mode of operation: 'stdout', 'inplace', or 'check'"
    ]
  <*> (optional . strOption . mconcat)
    [ long "config"
    , short 'c'
    , metavar "CONFIG"
    , help "Location of configuration file"
    ]
  <*> (switch . mconcat)
    [ long "unsafe"
    , short 'u'
    , help "Do formatting faster but without automatic detection of defects"
    ]
  <*> (fmap (fmap DynOption) . many . strOption . mconcat)
    [ long "ghc-opt"
    , short 'o'
    , metavar "OPT"
    , help "GHC options to enable (e.g. language extensions)"
    ]
  <*> (switch . mconcat)
    [ long "debug"
    , short 'd'
    , help "Output information useful for debugging"
    ]
  <*> (strArgument . mconcat)
    [ metavar "FILE"
    , help "Haskell source file to format"
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

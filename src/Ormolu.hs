-- | A formatter for Haskell source code.

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu
  ( ormolu
  , ormoluFile
  , ormoluStdin
  , Config (..)
  , defaultConfig
  , DynOption (..)
  , OrmoluException (..)
  , withPrettyOrmoluExceptions
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Debug.Trace
import Ormolu.Config
import Ormolu.Diff
import Ormolu.Exception
import Ormolu.Parser
import Ormolu.Parser.Result
import Ormolu.Printer
import Ormolu.Utils (showOutputable)
import System.IO (hGetContents, stdin)
import qualified CmdLineParser as GHC
import qualified Data.Text as T
import qualified SrcLoc as GHC

-- | Format a 'String', return formatted version as 'Text'.
--
-- The function
--
--     * Takes 'String' because that's what GHC parser accepts.
--     * Needs 'IO' because some functions from GHC that are necessary to
--       setup parsing context require 'IO'. There should be no visible
--       side-effects though.
--     * Takes file name just to use it in parse error messages.
--     * Throws 'OrmoluException'.

ormolu
  :: MonadIO m
  => Config                     -- ^ Ormolu configuration
  -> FilePath                   -- ^ Location of source file
  -> String                     -- ^ Input to format
  -> m Text
ormolu cfg path str = do
  (ws, result0) <-
    parseModule' cfg OrmoluParsingFailed path str
  when (cfgDebug cfg) $ do
    traceM "warnings:\n"
    traceM (concatMap showWarn ws)
    traceM (prettyPrintParseResult result0)
  -- NOTE We're forcing 'txt' here because otherwise errors (such as
  -- messages about not-yet-supported functionality) will be thrown later
  -- when we try to parse the rendered code back, inside of GHC monad
  -- wrapper which will lead to error messages presenting the exceptions as
  -- GHC bugs.
  let !txt = printModule result0
  when (not (cfgUnsafe cfg) || cfgCheckIdempotency cfg) $ do
    let pathRendered = path ++ "<rendered>"
    -- Parse the result of pretty-printing again and make sure that AST
    -- is the same as AST of original snippet module span positions.
    (_, result1) <-
      parseModule'
        cfg
        OrmoluOutputParsingFailed
        pathRendered
        (T.unpack txt)
    unless (cfgUnsafe cfg) $
      case diffParseResult result0 result1 of
        Same -> return ()
        Different ss -> liftIO $ throwIO (OrmoluASTDiffers path ss)
    -- Try re-formatting the formatted result to check if we get exactly
    -- the same output.
    when (cfgCheckIdempotency cfg) $
      let txt2 = printModule result1
       in case diffText txt txt2 pathRendered of
            Nothing -> return ()
            Just (loc, l, r) -> liftIO $
              throwIO (OrmoluNonIdempotentOutput loc l r)
  return txt

-- | Load a file and format it. The file stays intact and the rendered
-- version is returned as 'Text'.
--
-- > ormoluFile cfg path =
-- >   liftIO (readFile path) >>= ormolu cfg path

ormoluFile
  :: MonadIO m
  => Config                     -- ^ Ormolu configuration
  -> FilePath                   -- ^ Location of source file
  -> m Text                     -- ^ Resulting rendition
ormoluFile cfg path =
  liftIO (readFile path) >>= ormolu cfg path

-- | Read input from stdin and format it.
--
-- > ormoluStdin cfg =
-- >   liftIO (hGetContents stdin) >>= ormolu cfg "<stdin>"

ormoluStdin
  :: MonadIO m
  => Config                     -- ^ Ormolu configuration
  -> m Text                     -- ^ Resulting rendition
ormoluStdin cfg =
  liftIO (hGetContents stdin) >>= ormolu cfg "<stdin>"

----------------------------------------------------------------------------
-- Helpers

-- | A wrapper around 'parseModule'.

parseModule'
  :: MonadIO m
  => Config                     -- ^ Ormolu configuration
  -> (GHC.SrcSpan -> String -> OrmoluException)
     -- ^ How to obtain 'OrmoluException' to throw when parsing fails
  -> FilePath                   -- ^ File name to use in errors
  -> String                     -- ^ Actual input for the parser
  -> m ([GHC.Warn], ParseResult)
parseModule' cfg mkException path str = do
  (ws, r) <- parseModule cfg path str
  case r of
    Left (spn, err) -> liftIO $ throwIO (mkException spn err)
    Right x -> return (ws, x)

-- | Pretty-print a 'GHC.Warn'.

showWarn :: GHC.Warn -> String
showWarn (GHC.Warn reason l) = unlines
  [ showOutputable reason
  , showOutputable l
  ]

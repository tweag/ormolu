-- | A formatter for Haskell source code.

{-# LANGUAGE RecordWildCards #-}

module Ormolu
  ( ormolu
  , ormoluFile
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
import Ormolu.Anns
import Ormolu.CommentStream
import Ormolu.Config
import Ormolu.Diff
import Ormolu.Exception
import Ormolu.Parser
import Ormolu.Printer
import qualified CmdLineParser as GHC
import qualified Data.Text as T
import qualified GHC
import qualified Outputable as GHC

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
  (ws, (cstream0, anns0, parsedSrc0)) <-
    parseModule' cfg OrmoluParsingFailed path str
  when (cfgDebug cfg) $ do
    traceM "warnings:\n"
    traceM (concatMap showWarn ws)
    traceM "comment stream:\n"
    traceM (showCommentStream cstream0)
  let txt = printModule (cfgDebug cfg) cstream0 anns0 parsedSrc0
  -- Parse the result of pretty-printing again and make sure that AST is the
  -- same as AST of original snippet module span positions.
  unless (cfgUnsafe cfg) $ do
    (_, (cstream1, _, parsedSrc1)) <-
      parseModule' cfg OrmoluOutputParsingFailed "<rendered>" (T.unpack txt)
    when (diff (cstream0, parsedSrc0) (cstream1, parsedSrc1)) $
      liftIO $ throwIO (OrmoluASTDiffers str txt)
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
  -> m ([GHC.Warn], (CommentStream, Anns, GHC.ParsedSource))
     -- ^ Comment stream and parsed source
parseModule' Config {..} mkException path str = do
  (ws, r) <- parseModule cfgDynOptions path str
  case r of
    Left (spn, err) -> liftIO $ throwIO (mkException spn err)
    Right x -> return (ws, x)

-- | Pretty-print a 'GHC.Warn'.

showWarn :: GHC.Warn -> String
showWarn (GHC.Warn reason l) =
  showOutputable reason ++ "\n" ++ showOutputable l ++ "\n"

-- | Pretty-print a 'CommentStream'.

showCommentStream :: CommentStream -> String
showCommentStream (CommentStream xs) = unlines $
  showComment <$> xs
  where
    showComment (GHC.L l str) = showOutputable l ++ " " ++ show str

-- | Pretty-print an 'GHC.Outputable' thing.

showOutputable :: GHC.Outputable o => o -> String
showOutputable = GHC.showSDocUnsafe . GHC.ppr

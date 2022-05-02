{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | A formatter for Haskell source code.
module Ormolu
  ( ormolu,
    ormoluFile,
    ormoluStdin,
    Config (..),
    ColorMode (..),
    RegionIndices (..),
    SourceType (..),
    defaultConfig,
    detectSourceType,
    DynOption (..),
    OrmoluException (..),
    withPrettyOrmoluExceptions,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import qualified GHC.Driver.CmdLine as GHC
import qualified GHC.Types.SrcLoc as GHC
import Ormolu.Config
import Ormolu.Diff.ParseResult
import Ormolu.Diff.Text
import Ormolu.Exception
import Ormolu.Fixity
import Ormolu.Parser
import Ormolu.Parser.CommentStream (showCommentStream)
import Ormolu.Parser.Result
import Ormolu.Printer
import Ormolu.Utils (showOutputable)
import Ormolu.Utils.IO
import System.FilePath

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
--
-- __NOTE__: The caller is responsible for setting the appropriate value in
-- the 'cfgSourceType' field. Autodetection of source type won't happen
-- here, see 'detectSourceType'.
ormolu ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Location of source file
  FilePath ->
  -- | Input to format
  String ->
  m Text
ormolu cfgWithIndices path originalInput = do
  let totalLines = length (lines originalInput)
      cfg = regionIndicesToDeltas totalLines <$> cfgWithIndices
      fixityMap = buildFixityMap defaultStrategyThreshold (cfgDependencies cfg)
  (warnings, result0) <-
    parseModule' cfg fixityMap OrmoluParsingFailed path originalInput
  when (cfgDebug cfg) $ do
    traceM "warnings:\n"
    traceM (concatMap showWarn warnings)
    forM_ result0 $ \case
      ParsedSnippet r -> traceM . showCommentStream . prCommentStream $ r
      _ -> pure ()
  -- We're forcing 'formattedText' here because otherwise errors (such as
  -- messages about not-yet-supported functionality) will be thrown later
  -- when we try to parse the rendered code back, inside of GHC monad
  -- wrapper which will lead to error messages presenting the exceptions as
  -- GHC bugs.
  let !formattedText = printSnippets result0
  when (not (cfgUnsafe cfg) || cfgCheckIdempotence cfg) $ do
    -- Parse the result of pretty-printing again and make sure that AST
    -- is the same as AST of original snippet module span positions.
    (_, result1) <-
      parseModule'
        cfg
        fixityMap
        OrmoluOutputParsingFailed
        path
        (T.unpack formattedText)
    unless (cfgUnsafe cfg) . liftIO $ do
      let diff = case diffText (T.pack originalInput) formattedText path of
            Nothing -> error "AST differs, yet no changes have been introduced"
            Just x -> x
      when (length result0 /= length result1) $
        throwIO (OrmoluASTDiffers diff [])
      forM_ (result0 `zip` result1) $ \case
        (ParsedSnippet s, ParsedSnippet s') -> case diffParseResult s s' of
          Same -> return ()
          Different ss -> throwIO (OrmoluASTDiffers (selectSpans ss diff) ss)
        (RawSnippet {}, RawSnippet {}) -> pure ()
        _ -> throwIO (OrmoluASTDiffers diff [])
    -- Try re-formatting the formatted result to check if we get exactly
    -- the same output.
    when (cfgCheckIdempotence cfg) . liftIO $
      let reformattedText = printSnippets result1
       in case diffText formattedText reformattedText path of
            Nothing -> return ()
            Just diff -> throwIO (OrmoluNonIdempotentOutput diff)
  return formattedText

-- | Load a file and format it. The file stays intact and the rendered
-- version is returned as 'Text'.
--
-- __NOTE__: The caller is responsible for setting the appropriate value in
-- the 'cfgSourceType' field. Autodetection of source type won't happen
-- here, see 'detectSourceType'.
ormoluFile ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Location of source file
  FilePath ->
  -- | Resulting rendition
  m Text
ormoluFile cfg path =
  readFileUtf8 path >>= ormolu cfg path . T.unpack

-- | Read input from stdin and format it.
--
-- __NOTE__: The caller is responsible for setting the appropriate value in
-- the 'cfgSourceType' field. Autodetection of source type won't happen
-- here, see 'detectSourceType'.
ormoluStdin ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Resulting rendition
  m Text
ormoluStdin cfg =
  getContentsUtf8 >>= ormolu cfg "<stdin>" . T.unpack

----------------------------------------------------------------------------
-- Helpers

-- | A wrapper around 'parseModule'.
parseModule' ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | Fixity Map for operators
  LazyFixityMap ->
  -- | How to obtain 'OrmoluException' to throw when parsing fails
  (GHC.SrcSpan -> String -> OrmoluException) ->
  -- | File name to use in errors
  FilePath ->
  -- | Actual input for the parser
  String ->
  m ([GHC.Warn], [SourceSnippet])
parseModule' cfg fixityMap mkException path str = do
  (warnings, r) <- parseModule cfg fixityMap path str
  case r of
    Left (spn, err) -> liftIO $ throwIO (mkException spn err)
    Right x -> return (warnings, x)

-- | Pretty-print a 'GHC.Warn'.
showWarn :: GHC.Warn -> String
showWarn (GHC.Warn reason l) =
  unlines
    [ showOutputable reason,
      showOutputable l
    ]

-- | Detect 'SourceType' based on the file extension.
detectSourceType :: FilePath -> SourceType
detectSourceType mpath =
  if takeExtension mpath == ".hsig"
    then SignatureSource
    else ModuleSource

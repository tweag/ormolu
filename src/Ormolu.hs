{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | A formatter for Haskell source code. This module exposes the official
-- stable API, other modules may be not as reliable.
module Ormolu
  ( -- * Top-level formatting functions
    ormolu,
    ormoluFile,
    ormoluStdin,

    -- * Configuration
    Config (..),
    ColorMode (..),
    RegionIndices (..),
    SourceType (..),
    defaultConfig,
    detectSourceType,
    refineConfig,
    DynOption (..),

    -- * Cabal info
    CabalUtils.CabalSearchResult (..),
    CabalUtils.CabalInfo (..),
    CabalUtils.getCabalInfoForSourceFile,

    -- * Fixity overrides
    FixityMap,
    getFixityOverridesForSourceFile,

    -- * Working with exceptions
    OrmoluException (..),
    withPrettyOrmoluExceptions,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import qualified GHC.Driver.CmdLine as GHC
import GHC.Types.SrcLoc
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
import qualified Ormolu.Utils.Cabal as CabalUtils
import Ormolu.Utils.Fixity (getFixityOverridesForSourceFile)
import Ormolu.Utils.IO
import System.FilePath

-- | Format a 'Text'.
--
-- The function
--
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
  (MonadIO m) =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Location of source file
  FilePath ->
  -- | Input to format
  Text ->
  m Text
ormolu cfgWithIndices path originalInput = do
  let totalLines = length (T.lines originalInput)
      cfg = regionIndicesToDeltas totalLines <$> cfgWithIndices
      fixityMap =
        -- It is important to keep all arguments (but last) of
        -- 'buildFixityMap' constant (such as 'defaultStrategyThreshold'),
        -- otherwise it is going to break memoization.
        buildFixityMap
          defaultStrategyThreshold
          (cfgDependencies cfg) -- memoized on the set of dependencies
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
        formattedText
    unless (cfgUnsafe cfg) . liftIO $ do
      let diff = case diffText originalInput formattedText path of
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
  (MonadIO m) =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Location of source file
  FilePath ->
  -- | Resulting rendition
  m Text
ormoluFile cfg path =
  readFileUtf8 path >>= ormolu cfg path

-- | Read input from stdin and format it.
--
-- __NOTE__: The caller is responsible for setting the appropriate value in
-- the 'cfgSourceType' field. Autodetection of source type won't happen
-- here, see 'detectSourceType'.
ormoluStdin ::
  (MonadIO m) =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Resulting rendition
  m Text
ormoluStdin cfg =
  getContentsUtf8 >>= ormolu cfg "<stdin>"

-- | Refine a 'Config' by incorporating given 'SourceType', 'CabalInfo', and
-- fixity overrides 'FixityMap'. You can use 'detectSourceType' to deduce
-- 'SourceType' based on the file extension,
-- 'CabalUtils.getCabalInfoForSourceFile' to obtain 'CabalInfo' and
-- 'getFixityOverridesForSourceFile' for 'FixityMap'.
--
-- @since 0.5.3.0
refineConfig ::
  -- | Source type to use
  SourceType ->
  -- | Cabal info for the file, if available
  Maybe CabalUtils.CabalInfo ->
  -- | Fixity overrides, if available
  Maybe FixityMap ->
  -- | 'Config' to refine
  Config region ->
  -- | Refined 'Config'
  Config region
refineConfig sourceType mcabalInfo mfixityOverrides rawConfig =
  rawConfig
    { cfgDynOptions = cfgDynOptions rawConfig ++ dynOptsFromCabal,
      cfgFixityOverrides =
        Map.unionWith (<>) (cfgFixityOverrides rawConfig) fixityOverrides,
      cfgDependencies =
        Set.union (cfgDependencies rawConfig) depsFromCabal,
      cfgSourceType = sourceType
    }
  where
    fixityOverrides =
      case mfixityOverrides of
        Nothing -> Map.empty
        Just x -> x
    (dynOptsFromCabal, depsFromCabal) =
      case mcabalInfo of
        Nothing -> ([], Set.empty)
        Just CabalUtils.CabalInfo {..} ->
          -- It makes sense to take into account the operator info for the
          -- package itself if we know it, as if it were its own
          -- dependency.
          (ciDynOpts, Set.insert ciPackageName ciDependencies)

----------------------------------------------------------------------------
-- Helpers

-- | A wrapper around 'parseModule'.
parseModule' ::
  (MonadIO m) =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | Fixity Map for operators
  LazyFixityMap ->
  -- | How to obtain 'OrmoluException' to throw when parsing fails
  (SrcSpan -> String -> OrmoluException) ->
  -- | File name to use in errors
  FilePath ->
  -- | Actual input for the parser
  Text ->
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
      unLoc l
    ]

-- | Detect 'SourceType' based on the file extension.
detectSourceType :: FilePath -> SourceType
detectSourceType mpath =
  if takeExtension mpath == ".hsig"
    then SignatureSource
    else ModuleSource

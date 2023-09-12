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
    CabalUtils.CabalInfo (..),
    CabalUtils.StanzaInfo (..),
    CabalUtils.defaultStanzaInfo,
    CabalUtils.StanzaInfoMap,
    CabalUtils.lookupStanzaInfo,
    CabalUtils.getCabalInfoForSourceFile,

    -- * Fixity overrides and module re-exports
    FixityOverrides,
    defaultFixityOverrides,
    ModuleReexports,
    defaultModuleReexports,
    getDotOrmoluForSourceFile,

    -- * Working with exceptions
    OrmoluException (..),
    withPrettyOrmoluExceptions,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace
import Distribution.PackageDescription (PackageName)
import GHC.Driver.CmdLine qualified as GHC
import GHC.Types.SrcLoc
import Ormolu.Config
import Ormolu.Diff.ParseResult
import Ormolu.Diff.Text
import Ormolu.Exception
import Ormolu.Fixity
import Ormolu.Parser
import Ormolu.Parser.CommentStream (CommentStream (..))
import Ormolu.Parser.Result
import Ormolu.Printer
import Ormolu.Utils (showOutputable)
import Ormolu.Utils.Cabal qualified as CabalUtils
import Ormolu.Utils.Fixity (getDotOrmoluForSourceFile)
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
        packageFixityMap
          (overapproximatedDependencies cfg) -- memoized on the set of dependencies
  when (cfgDebug cfg) $ do
    traceM $ unwords ["*** CONFIG ***", show cfg]
  (warnings, result0) <-
    parseModule' cfg fixityMap OrmoluParsingFailed path originalInput
  when (cfgDebug cfg) $ do
    forM_ warnings $ \(GHC.Warn reason (L loc msg)) ->
      traceM $ unwords ["*** WARNING ***", showOutputable loc, msg, showOutputable reason]
    forM_ result0 $ \case
      ParsedSnippet r -> do
        let CommentStream comments = prCommentStream r
        forM_ comments $ \(L loc comment) ->
          traceM $ unwords ["*** COMMENT ***", showOutputable loc, show comment]
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

-- | Refine a 'Config' by incorporating the given information.
--
-- You can use 'detectSourceType' to deduce 'SourceType' based on the file extension,
-- 'CabalUtils.getCabalInfoForSourceFile' to obtain 'PackageName' and
-- 'CabalUtils.StanzaInfo', and 'getFixityOverridesForSourceFile' for 'FixityMap'.
--
-- @since 0.8.0.0
refineConfig ::
  -- | Source type to use
  SourceType ->
  -- | Name of the package, if available
  Maybe PackageName ->
  -- | Stanza information for the source file, if available
  Maybe CabalUtils.StanzaInfo ->
  -- | Fixity overrides, if available
  Maybe FixityOverrides ->
  -- | Module re-exports, if available
  Maybe ModuleReexports ->
  -- | 'Config' to refine
  Config region ->
  -- | Refined 'Config'
  Config region
refineConfig sourceType mPackageName mStanzaInfo mfixityOverrides mreexports rawConfig =
  rawConfig
    { cfgDynOptions = cfgDynOptions rawConfig ++ dynOptsFromCabal,
      cfgFixityOverrides =
        FixityOverrides $
          Map.unions
            [ unFixityOverrides fixityOverrides,
              unFixityOverrides (cfgFixityOverrides rawConfig),
              unFixityOverrides defaultFixityOverrides
            ],
      cfgModuleReexports =
        ModuleReexports $
          Map.unionsWith
            (<>)
            [ unModuleReexports reexports,
              unModuleReexports (cfgModuleReexports rawConfig),
              unModuleReexports defaultModuleReexports
            ],
      cfgDependencies =
        Set.union (cfgDependencies rawConfig) depsFromCabal,
      cfgSourceType = sourceType
    }
  where
    fixityOverrides = fromMaybe defaultFixityOverrides mfixityOverrides
    reexports = fromMaybe defaultModuleReexports mreexports
    CabalUtils.StanzaInfo {..} = fromMaybe CabalUtils.defaultStanzaInfo mStanzaInfo
    dynOptsFromCabal = siDynOpts
    depsFromCabal =
      case mPackageName of
        Nothing -> siDependencies
        -- It makes sense to take into account the operator info for the
        -- package itself if we know it, as if it were its own dependency.
        Just package -> Set.insert package siDependencies

----------------------------------------------------------------------------
-- Helpers

-- | A wrapper around 'parseModule'.
parseModule' ::
  (MonadIO m) =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | Fixity Map for operators
  PackageFixityMap ->
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

-- | Detect 'SourceType' based on the file extension.
detectSourceType :: FilePath -> SourceType
detectSourceType mpath =
  if takeExtension mpath == ".hsig"
    then SignatureSource
    else ModuleSource

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | A formatter for Haskell source code. This module exposes the official
-- stable API; other modules may not be as reliable.
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
import Data.Choice qualified as Choice
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T.Utf8
import Debug.Trace
import GHC.Driver.Errors.Types
import GHC.Hs (HsModule (..), locA)
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Error
import Ormolu.Comments.Invariants
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
import System.FilePath

-- | Format a 'Text'.
--
-- The function:
--
--     * Needs 'IO' because some GHC functions that are necessary to set up
--       the parsing context require 'IO'. There should be no visible
--       side effects, though.
--     * Takes a file name only to use it in parse error messages.
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
    forM_ warnings $ \driverMsg -> do
      let driverMsgSDoc = formatBulleted $ diagnosticMessage defaultOpts driverMsg
      traceM $ unwords ["*** WARNING ***", showOutputable driverMsgSDoc]
    forM_ result0 $ \case
      ParsedSnippet r -> do
        let CommentStream comments = prCommentStream r
        forM_ comments $ \(L loc comment) ->
          traceM $ unwords ["*** COMMENT ***", showOutputable loc, show comment]
      _ -> pure ()
  -- We force 'formattedText' here because otherwise errors (such as
  -- messages about not-yet-supported functionality) would be thrown later,
  -- when we try to parse the rendered code back inside the GHC monad
  -- wrapper, which would lead to error messages presenting the exceptions
  -- as GHC bugs.
  let printed =
        printSnippetsWithPlacements (Choice.fromBool (cfgDebug cfg)) result0
      !formattedText = T.concat (fst <$> printed)
  -- Every comment of the input should come out exactly once, and in the
  -- order it went in. The AST check below does not cover this: it compares
  -- the comment streams as multisets, and the comments that travel with
  -- pragmas are not in the stream at all.
  unless (cfgUnsafe cfg) . liftIO $ do
    let violations =
          concat
            [ checkCommentInvariants
                (getLoc <$> inputComments r)
                (reorderableSpans (prParsedSource r))
                placements
            | (ParsedSnippet r, (_, placements)) <- result0 `zip` printed
            ]
        -- Imports are sorted and merged, so a comment attached to one of
        -- them may legitimately come out in a different order than it went
        -- in.
        reorderableSpans hsmod =
          [ spn
          | L l _ <- hsmodImports hsmod,
            Just spn <- [srcSpanToRealSrcSpan (locA l)]
          ]
    unless (null violations) $
      throwIO (OrmoluCommentInvariantsViolated path violations)
  when (not (cfgUnsafe cfg) || cfgCheckIdempotence cfg) $ do
    -- Parse the result of pretty-printing again and make sure that its AST
    -- is the same as the AST of the original snippet, modulo span
    -- positions.
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
      let reformattedText =
            printSnippets (Choice.fromBool (cfgDebug cfg)) result1
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
  liftIO (T.Utf8.readFile path) >>= ormolu cfg path

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
  liftIO T.Utf8.getContents >>= ormolu cfg "<stdin>"

-- | Refine a 'Config' by incorporating the given 'SourceType', 'CabalInfo',
-- and fixity overrides 'FixityMap'. You can use 'detectSourceType' to deduce
-- the 'SourceType' from the file extension,
-- 'CabalUtils.getCabalInfoForSourceFile' to obtain the 'CabalInfo', and
-- 'getFixityOverridesForSourceFile' for the 'FixityMap'.
--
-- @since 0.5.3.0
refineConfig ::
  -- | Source type to use
  SourceType ->
  -- | Cabal info for the file, if available
  Maybe CabalUtils.CabalInfo ->
  -- | Fixity overrides, if available
  Maybe FixityOverrides ->
  -- | Module re-exports, if available
  Maybe ModuleReexports ->
  -- | 'Config' to refine
  Config region ->
  -- | Refined 'Config'
  Config region
refineConfig sourceType mcabalInfo mfixityOverrides mreexports rawConfig =
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
    (dynOptsFromCabal, depsFromCabal) =
      case mcabalInfo of
        Nothing ->
          -- If no cabal info is provided, assume base as a dependency by
          -- default.
          ([], defaultDependencies)
        Just CabalUtils.CabalInfo {..} ->
          -- It makes sense to take into account the operator info for the
          -- package itself if we know it, as if it were its own dependency.
          (ciDynOpts, Set.insert ciPackageName ciDependencies)

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
  m (DriverMessages, [SourceSnippet])
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

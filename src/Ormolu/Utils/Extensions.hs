{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Utils.Extensions
  ( Extension (..),
    getExtensionsFromCabalFile,
    findCabalFile,
    getCabalExtensionDynOptions,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (maybeToList)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import qualified Distribution.Types.CondTree as CT
import Distribution.Utils.Path (getSymbolicPath)
import Language.Haskell.Extension
import Ormolu.Config
import Ormolu.Exception
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

-- | Get a map from Haskell source file paths (without any extensions)
-- to its default language extensions
getExtensionsFromCabalFile ::
  MonadIO m =>
  -- | Path to cabal file
  FilePath ->
  m (Map FilePath [DynOption])
getExtensionsFromCabalFile cabalFile = liftIO $ do
  GenericPackageDescription {..} <-
    parseGenericPackageDescriptionMaybe <$> B.readFile cabalFile >>= \case
      Just gpd -> pure gpd
      Nothing -> throwIO $ OrmoluCabalFileParsingFailed cabalFile
  let lib = maybeToList condLibrary
      sublibs = snd <$> condSubLibraries
  pure . M.unions . concat $
    [ buildMap extractFromLibrary <$> lib ++ sublibs,
      buildMap extractFromExecutable . snd <$> condExecutables,
      buildMap extractFromTestSuite . snd <$> condTestSuites,
      buildMap extractFromBenchmark . snd <$> condBenchmarks
    ]
  where
    buildMap f a = let (files, exts) = f mergedA in M.fromList $ (,exts) <$> files
      where
        (mergedA, _) = CT.ignoreConditions a

    extractFromBuildInfo extraModules BuildInfo {..} = (,exts) $ do
      m <- extraModules ++ (ModuleName.toFilePath <$> otherModules)
      (takeDirectory cabalFile </>) <$> prependSrcDirs (dropExtensions m)
      where
        prependSrcDirs f
          | null hsSourceDirs = [f]
          | otherwise = (</> f) . getSymbolicPath <$> hsSourceDirs
        exts = maybe [] langExt defaultLanguage ++ fmap extToDynOption defaultExtensions
        langExt =
          pure . DynOption . ("-X" <>) . \case
            UnknownLanguage lan -> lan
            lan -> show lan
        extToDynOption =
          DynOption . \case
            EnableExtension e -> "-X" ++ show e
            DisableExtension e -> "-XNo" ++ show e
            UnknownExtension e -> "-X" ++ e

    extractFromLibrary Library {..} =
      extractFromBuildInfo (ModuleName.toFilePath <$> exposedModules) libBuildInfo
    extractFromExecutable Executable {..} =
      extractFromBuildInfo [modulePath] buildInfo
    extractFromTestSuite TestSuite {..} =
      extractFromBuildInfo mainPath testBuildInfo
      where
        mainPath = case testInterface of
          TestSuiteExeV10 _ p -> [p]
          TestSuiteLibV09 _ p -> [ModuleName.toFilePath p]
          TestSuiteUnsupported {} -> []
    extractFromBenchmark Benchmark {..} =
      extractFromBuildInfo mainPath benchmarkBuildInfo
      where
        mainPath = case benchmarkInterface of
          BenchmarkExeV10 _ p -> [p]
          BenchmarkUnsupported {} -> []

-- | Find the path to an appropriate .cabal file for a Haskell
-- source file, if available
findCabalFile ::
  MonadIO m =>
  -- | Absolute path to a Haskell source file in a project with a .cabal file
  FilePath ->
  m (Maybe FilePath)
findCabalFile p = liftIO $ do
  let parentDir = takeDirectory p
  dirEntries <-
    listDirectory parentDir `catch` \case
      (isDoesNotExistError -> True) -> pure []
      e -> throwIO e
  let findDotCabal = \case
        [] -> pure Nothing
        e : es
          | takeExtension e == ".cabal" ->
              doesFileExist (parentDir </> e) >>= \case
                True -> pure $ Just e
                False -> findDotCabal es
        _ : es -> findDotCabal es
  findDotCabal dirEntries >>= \case
    Just cabalFile -> pure . Just $ parentDir </> cabalFile
    Nothing ->
      if isDrive parentDir
        then pure Nothing
        else findCabalFile parentDir

-- | Get the default language extensions of a Haskell source file.
-- The .cabal file can be provided explicitly or auto-detected.
getCabalExtensionDynOptions ::
  MonadIO m =>
  -- | Haskell source file
  FilePath ->
  m [DynOption]
getCabalExtensionDynOptions sourceFile' = liftIO $ do
  sourceFile <- makeAbsolute sourceFile'
  findCabalFile sourceFile >>= \case
    Just cabalFile -> do
      extsByFile <- getExtensionsFromCabalFile cabalFile
      case M.lookup (dropExtensions sourceFile) extsByFile of
        Just exts -> pure exts
        Nothing -> do
          relativeCabalFile <- makeRelativeToCurrentDirectory cabalFile
          note $
            "Found .cabal file "
              <> relativeCabalFile
              <> ", but it did not mention "
              <> sourceFile'
    Nothing -> note $ "Could not find a .cabal file for " <> sourceFile'
  where
    note msg = [] <$ hPutStrLn stderr msg

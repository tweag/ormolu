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
import Data.List (find)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (maybeToList)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Language.Haskell.Extension
import Ormolu.Config
import Ormolu.Exception
import System.Directory
import System.FilePath
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
    buildMap f a = let (files, exts) = f (condTreeData a) in M.fromList $ (,exts) <$> files

    extractFromBuildInfo extraModules BuildInfo {..} = (,exts) $ do
      m <- extraModules ++ (ModuleName.toFilePath <$> otherModules)
      (takeDirectory cabalFile </>) . (</> dropExtensions m) <$> hsSourceDirs
      where
        exts = maybe [] langExt defaultLanguage ++ fmap extToDynOption defaultExtensions
        langExt =
          pure . DynOption . \case
            Haskell98 -> "-XHaskell98"
            Haskell2010 -> "-XHaskell2010"
            UnknownLanguage lan -> "-X" ++ lan
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
  ps <-
    listDirectory parentDir `catch` \case
      (isDoesNotExistError -> True) -> pure []
      e -> throwIO e
  case find ((== ".cabal") . takeExtension) ps of
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
  mCabalFile <- findCabalFile sourceFile
  case mCabalFile of
    Just cabalFile -> do
      extsByFile <- getExtensionsFromCabalFile cabalFile
      pure $ M.findWithDefault [] (dropExtensions sourceFile) extsByFile
    Nothing -> pure []

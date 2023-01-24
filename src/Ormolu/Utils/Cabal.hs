{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Utils.Cabal
  ( CabalSearchResult (..),
    CabalInfo (..),
    Extension (..),
    getCabalInfoForSourceFile,
    findCabalFile,
    parseCabalInfo,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.IORef
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
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
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)

-- | The result of searching for a @.cabal@ file.
--
-- @since 0.5.3.0
data CabalSearchResult
  = -- | Cabal file could not be found
    CabalNotFound
  | -- | Cabal file was found, but it did not mention the source file in
    -- question
    CabalDidNotMention CabalInfo
  | -- | Cabal file was found and it mentions the source file in question
    CabalFound CabalInfo
  deriving (Eq, Show)

-- | Cabal information of interest to Ormolu.
data CabalInfo = CabalInfo
  { -- | Package name
    ciPackageName :: !PackageName,
    -- | Extension and language settings in the form of 'DynOption's
    ciDynOpts :: ![DynOption],
    -- | Direct dependencies
    ciDependencies :: !(Set PackageName),
    -- | Absolute path to the cabal file
    ciCabalFilePath :: !FilePath
  }
  deriving (Eq, Show)

-- | Locate a @.cabal@ file corresponding to the given Haskell source file
-- and obtain 'CabalInfo' from it.
getCabalInfoForSourceFile ::
  (MonadIO m) =>
  -- | Haskell source file
  FilePath ->
  -- | Extracted cabal info, if any
  m CabalSearchResult
getCabalInfoForSourceFile sourceFile = liftIO $ do
  findCabalFile sourceFile >>= \case
    Just cabalFile -> do
      (mentioned, cabalInfo) <- parseCabalInfo cabalFile sourceFile
      return
        ( if mentioned
            then CabalFound cabalInfo
            else CabalDidNotMention cabalInfo
        )
    Nothing -> return CabalNotFound

-- | Find the path to an appropriate .cabal file for a Haskell source file,
-- if available.
findCabalFile ::
  (MonadIO m) =>
  -- | Path to a Haskell source file in a project with a .cabal file
  FilePath ->
  -- | Absolute path to the .cabal file if available
  m (Maybe FilePath)
findCabalFile sourceFile = liftIO $ do
  parentDir <- takeDirectory <$> makeAbsolute sourceFile
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

-- | Parsed cabal file information to be shared across multiple source files.
data CachedCabalFile = CachedCabalFile
  { -- | Parsed generic package description.
    genericPackageDescription :: GenericPackageDescription,
    -- | Map from Haskell source file paths (without any extensions) to the
    -- corresponding 'DynOption's and dependencies.
    extensionsAndDeps :: Map FilePath ([DynOption], [PackageName])
  }
  deriving (Show)

-- | Cache ref that stores 'CachedCabalFile' per cabal file.
cabalCacheRef :: IORef (Map FilePath CachedCabalFile)
cabalCacheRef = unsafePerformIO $ newIORef M.empty
{-# NOINLINE cabalCacheRef #-}

-- | Parse 'CabalInfo' from a .cabal file at the given 'FilePath'.
parseCabalInfo ::
  (MonadIO m) =>
  -- | Location of the .cabal file
  FilePath ->
  -- | Location of the source file we are formatting
  FilePath ->
  -- | Indication if the source file was mentioned in the Cabal file and the
  -- extracted 'CabalInfo'
  m (Bool, CabalInfo)
parseCabalInfo cabalFileAsGiven sourceFileAsGiven = liftIO $ do
  cabalFile <- makeAbsolute cabalFileAsGiven
  sourceFileAbs <- makeAbsolute sourceFileAsGiven
  cabalCache <- readIORef cabalCacheRef
  CachedCabalFile {..} <- whenNothing (M.lookup cabalFile cabalCache) $ do
    cabalFileBs <- B.readFile cabalFile
    genericPackageDescription <-
      whenNothing (parseGenericPackageDescriptionMaybe cabalFileBs) $
        throwIO (OrmoluCabalFileParsingFailed cabalFile)
    let extensionsAndDeps =
          getExtensionAndDepsMap cabalFile genericPackageDescription
        cachedCabalFile = CachedCabalFile {..}
    atomicModifyIORef cabalCacheRef $
      (,cachedCabalFile) . M.insert cabalFile cachedCabalFile
  let (dynOpts, dependencies, mentioned) =
        case M.lookup (dropExtensions sourceFileAbs) extensionsAndDeps of
          Nothing -> ([], [], False)
          Just (dynOpts', dependencies') -> (dynOpts', dependencies', True)
      pdesc = packageDescription genericPackageDescription
  return
    ( mentioned,
      CabalInfo
        { ciPackageName = pkgName (package pdesc),
          ciDynOpts = dynOpts,
          ciDependencies = Set.fromList dependencies,
          ciCabalFilePath = cabalFile
        }
    )
  where
    whenNothing :: (Monad m) => Maybe a -> m a -> m a
    whenNothing maya ma = maybe ma pure maya

-- | Get a map from Haskell source file paths (without any extensions) to
-- the corresponding 'DynOption's and dependencies.
getExtensionAndDepsMap ::
  -- | Path to the cabal file
  FilePath ->
  -- | Parsed generic package description
  GenericPackageDescription ->
  Map FilePath ([DynOption], [PackageName])
getExtensionAndDepsMap cabalFile GenericPackageDescription {..} =
  M.unions . concat $
    [ buildMap extractFromLibrary <$> lib ++ sublibs,
      buildMap extractFromExecutable . snd <$> condExecutables,
      buildMap extractFromTestSuite . snd <$> condTestSuites,
      buildMap extractFromBenchmark . snd <$> condBenchmarks
    ]
  where
    lib = maybeToList condLibrary
    sublibs = snd <$> condSubLibraries

    buildMap f a = M.fromList ((,extsAndDeps) <$> files)
      where
        (mergedA, _) = CT.ignoreConditions a
        (files, extsAndDeps) = f mergedA

    extractFromBuildInfo extraModules BuildInfo {..} = (,(exts, deps)) $ do
      m <- extraModules ++ (ModuleName.toFilePath <$> otherModules)
      normalise . (takeDirectory cabalFile </>) <$> prependSrcDirs (dropExtensions m)
      where
        prependSrcDirs f
          | null hsSourceDirs = [f]
          | otherwise = (</> f) . getSymbolicPath <$> hsSourceDirs
        deps = depPkgName <$> targetBuildDepends
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

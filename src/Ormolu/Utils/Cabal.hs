{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.ByteString qualified as B
import Data.IORef
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.ModuleName qualified as ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Types.CondTree qualified as CT
import Distribution.Utils.Path (getSymbolicPath)
import Language.Haskell.Extension
import Ormolu.Config
import Ormolu.Exception
import Ormolu.Utils.IO (findClosestFileSatisfying)
import System.Directory
import System.FilePath
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
getCabalInfoForSourceFile sourceFile =
  liftIO (findCabalFile sourceFile) >>= \case
    Just cabalFile -> do
      (mentioned, cabalInfo) <- parseCabalInfo cabalFile sourceFile
      return
        ( if mentioned
            then CabalFound cabalInfo
            else CabalDidNotMention cabalInfo
        )
    Nothing -> return CabalNotFound

-- | Find the path to an appropriate @.cabal@ file for a Haskell source
-- file, if available.
findCabalFile ::
  (MonadIO m) =>
  -- | Path to a Haskell source file in a project with a @.cabal@ file
  FilePath ->
  -- | Absolute path to the @.cabal@ file, if available
  m (Maybe FilePath)
findCabalFile = findClosestFileSatisfying $ \x ->
  takeExtension x == ".cabal"

-- | Parsed cabal file information to be shared across multiple source files.
data CachedCabalFile = CachedCabalFile
  { -- | Parsed generic package description.
    genericPackageDescription :: GenericPackageDescription,
    -- | Map from Haskell source file paths (without any extensions) to the
    -- corresponding 'DynOption's and dependencies.
    extensionsAndDeps :: Map FilePath ([DynOption], [PackageName])
  }
  deriving (Show)

-- | Cache ref that stores 'CachedCabalFile' per Cabal file.
cacheRef :: IORef (Map FilePath CachedCabalFile)
cacheRef = unsafePerformIO $ newIORef M.empty
{-# NOINLINE cacheRef #-}

-- | Parse 'CabalInfo' from a @.cabal@ file at the given 'FilePath'.
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
  cabalCache <- readIORef cacheRef
  CachedCabalFile {..} <- whenNothing (M.lookup cabalFile cabalCache) $ do
    cabalFileBs <- B.readFile cabalFile
    genericPackageDescription <-
      whenLeft (snd . runParseResult $ parseGenericPackageDescription cabalFileBs) $
        throwIO . OrmoluCabalFileParsingFailed cabalFile . snd
    let extensionsAndDeps =
          getExtensionAndDepsMap cabalFile genericPackageDescription
        cachedCabalFile = CachedCabalFile {..}
    atomicModifyIORef cacheRef $
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
    whenNothing :: (Applicative f) => Maybe a -> f a -> f a
    whenNothing maya ma = maybe ma pure maya
    whenLeft :: (Applicative f) => Either e a -> (e -> f a) -> f a
    whenLeft eitha ma = either ma pure eitha

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

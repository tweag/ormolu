{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Utils.Cabal
  ( CabalInfo (..),
    StanzaInfo (..),
    defaultStanzaInfo,
    StanzaInfoMap,
    lookupStanzaInfo,
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
import Ormolu.Fixity
import Ormolu.Utils.IO (findClosestFileSatisfying, withIORefCache)
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

-- | Cabal information of interest to Ormolu.
data CabalInfo = CabalInfo
  { -- | Package name
    ciPackageName :: !PackageName,
    -- | Absolute path to the cabal file
    ciCabalFilePath :: !FilePath,
    -- | Stanza information for all source files mentioned in the cabal file
    ciStanzaInfoMap :: !StanzaInfoMap
  }
  deriving (Eq, Show)

-- | Information from the stanza corresponding to a given source file.
data StanzaInfo = StanzaInfo
  { -- | Extension and language settings in the form of 'DynOption's
    siDynOpts :: ![DynOption],
    -- | Direct dependencies
    siDependencies :: !(Set PackageName)
  }
  deriving (Eq, Show)

defaultStanzaInfo :: StanzaInfo
defaultStanzaInfo =
  StanzaInfo
    { siDynOpts = [],
      siDependencies = defaultDependencies
    }

-- | Map from source files (absolute path without extensions) to the corresponding stanza information.
newtype StanzaInfoMap = StanzaInfoMap (Map FilePath StanzaInfo)
  deriving (Eq, Show)

-- | Look up the given source file in the 'StanzaInfoMap'.
lookupStanzaInfo :: FilePath -> StanzaInfoMap -> IO (Maybe StanzaInfo)
lookupStanzaInfo path (StanzaInfoMap m) = do
  absPath <- makeAbsolute path
  pure $ M.lookup (dropExtensions absPath) m

-- | Locate a @.cabal@ file corresponding to the given Haskell source file
-- and obtain 'CabalInfo' from it.
getCabalInfoForSourceFile ::
  (MonadIO m) =>
  -- | Haskell source file
  FilePath ->
  -- | Extracted cabal info, if any
  m (Maybe CabalInfo)
getCabalInfoForSourceFile sourceFile =
  liftIO (findCabalFile sourceFile) >>= traverse parseCabalInfo

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

-- | Cache ref that stores 'CabalInfo' per Cabal file path.
cacheRef :: IORef (Map FilePath CabalInfo)
cacheRef = unsafePerformIO $ newIORef M.empty
{-# NOINLINE cacheRef #-}

-- | Parse 'CabalInfo' from a @.cabal@ file at the given 'FilePath'.
parseCabalInfo ::
  (MonadIO m) =>
  -- | Location of the .cabal file
  FilePath ->
  m CabalInfo
parseCabalInfo cabalFileAsGiven = liftIO $ do
  cabalFile <- makeAbsolute cabalFileAsGiven
  withIORefCache cacheRef cabalFile $ do
    cabalFileBs <- B.readFile cabalFile
    case snd . runParseResult . parseGenericPackageDescription $ cabalFileBs of
      Right genericPackageDescription ->
        pure
          CabalInfo
            { ciPackageName = pkgName . package . packageDescription $ genericPackageDescription,
              ciCabalFilePath = cabalFile,
              ciStanzaInfoMap = toStanzaInfoMap cabalFile genericPackageDescription
            }
      Left (_, e) -> throwIO $ OrmoluCabalFileParsingFailed cabalFile e

-- | Get a map from Haskell source file paths (without any extensions) to
-- the corresponding 'DynOption's and dependencies.
toStanzaInfoMap ::
  -- | Path to the cabal file
  FilePath ->
  -- | Parsed generic package description
  GenericPackageDescription ->
  StanzaInfoMap
toStanzaInfoMap cabalFile GenericPackageDescription {..} =
  StanzaInfoMap . M.unions . concat $
    [ buildMap extractFromLibrary <$> lib ++ sublibs,
      buildMap extractFromExecutable . snd <$> condExecutables,
      buildMap extractFromTestSuite . snd <$> condTestSuites,
      buildMap extractFromBenchmark . snd <$> condBenchmarks
    ]
  where
    lib = maybeToList condLibrary
    sublibs = snd <$> condSubLibraries

    buildMap f a = M.fromList ((,stanzaInfo) <$> files)
      where
        (mergedA, _) = CT.ignoreConditions a
        (files, stanzaInfo) = f mergedA

    extractFromBuildInfo extraModules BuildInfo {..} = (,stanzaInfo) $ do
      m <- extraModules ++ (ModuleName.toFilePath <$> otherModules)
      normalise . (takeDirectory cabalFile </>) <$> prependSrcDirs (dropExtensions m)
      where
        prependSrcDirs f
          | null hsSourceDirs = [f]
          | otherwise = (</> f) . getSymbolicPath <$> hsSourceDirs
        stanzaInfo =
          StanzaInfo
            { siDynOpts = maybe [] langExt defaultLanguage ++ fmap extToDynOption defaultExtensions,
              siDependencies = Set.fromList $ map depPkgName targetBuildDepends
            }
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

{-# LANGUAGE LambdaCase #-}

module Ormolu.Utils.Fixity
  ( getDotOrmoluForSourceFile,
    parseFixityDeclarationStr,
    parseModuleReexportDeclarationStr,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Distribution.ModuleName (ModuleName)
import Ormolu.Exception
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Ormolu.Utils.IO (findClosestFileSatisfying, readFileUtf8)
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (errorBundlePretty)

-- | Attempt to locate and parse an @.ormolu@ file. If it does not exist,
-- default fixity map and module reexports are returned. This function
-- maintains a cache of fixity overrides and module re-exports where cabal
-- file paths act as keys.
getDotOrmoluForSourceFile ::
  (MonadIO m) =>
  -- | 'CabalInfo' already obtained for this source file
  FilePath ->
  m (FixityOverrides, ModuleReexports)
getDotOrmoluForSourceFile sourceFile =
  liftIO (findDotOrmoluFile sourceFile) >>= \case
    Just dotOrmoluFile -> liftIO $ do
      cache <- readIORef cacheRef
      case Map.lookup dotOrmoluFile cache of
        Nothing -> do
          dotOrmoluRelative <- makeRelativeToCurrentDirectory dotOrmoluFile
          contents <- readFileUtf8 dotOrmoluFile
          case parseDotOrmolu dotOrmoluRelative contents of
            Left errorBundle ->
              throwIO (OrmoluFixityOverridesParseError errorBundle)
            Right x -> do
              modifyIORef' cacheRef (Map.insert dotOrmoluFile x)
              return x
        Just x -> return x
    Nothing -> return (defaultFixityOverrides, defaultModuleReexports)

-- | Find the path to an appropriate @.ormolu@ file for a Haskell source
-- file, if available.
findDotOrmoluFile ::
  (MonadIO m) =>
  -- | Path to a Haskell source file
  FilePath ->
  -- | Absolute path to the closest @.ormolu@ file, if available
  m (Maybe FilePath)
findDotOrmoluFile = findClosestFileSatisfying $ \x ->
  x == ".ormolu"

-- | Cache ref that maps names of @.ormolu@ files to their contents.
cacheRef :: IORef (Map FilePath (FixityOverrides, ModuleReexports))
cacheRef = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE cacheRef #-}

-- | A wrapper around 'parseFixityDeclaration' for parsing individual fixity
-- definitions.
parseFixityDeclarationStr ::
  -- | Input to parse
  String ->
  -- | Parse result
  Either String [(OpName, FixityInfo)]
parseFixityDeclarationStr =
  first errorBundlePretty . parseFixityDeclaration . T.pack

-- | A wrapper around 'parseModuleReexportDeclaration' for parsing
-- a individual module reexport.
parseModuleReexportDeclarationStr ::
  -- | Input to parse
  String ->
  -- | Parse result
  Either String (ModuleName, NonEmpty ModuleName)
parseModuleReexportDeclarationStr =
  first errorBundlePretty . parseModuleReexportDeclaration . T.pack

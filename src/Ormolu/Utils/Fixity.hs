{-# LANGUAGE RecordWildCards #-}

module Ormolu.Utils.Fixity
  ( getFixityOverridesForSourceFile,
    parseFixityDeclarationStr,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ormolu.Exception
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Ormolu.Utils.Cabal
import Ormolu.Utils.IO (readFileUtf8)
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (errorBundlePretty)

-- | Cache ref that stores fixity overrides per cabal file.
cacheRef :: IORef (Map FilePath FixityMap)
cacheRef = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE cacheRef #-}

-- | Attempt to locate and parse a @.ormolu@ file. If it does not exist,
-- empty fixity map is returned. This function maintains a cache of fixity
-- overrides where cabal file paths act as keys.
getFixityOverridesForSourceFile ::
  MonadIO m =>
  -- | 'CabalInfo' already obtained for this source file
  CabalInfo ->
  m FixityMap
getFixityOverridesForSourceFile CabalInfo {..} = liftIO $ do
  case ciCabalFilePath of
    Nothing -> return Map.empty
    Just cabalPath -> do
      cache <- readIORef cacheRef
      case Map.lookup cabalPath cache of
        Nothing -> do
          let dotOrmolu = replaceFileName cabalPath ".ormolu"
          exists <- doesFileExist dotOrmolu
          if exists
            then do
              dotOrmoluRelative <- makeRelativeToCurrentDirectory dotOrmolu
              contents <- readFileUtf8 dotOrmolu
              case parseFixityMap dotOrmoluRelative contents of
                Left errorBundle ->
                  throwIO (OrmoluFixityOverridesParseError errorBundle)
                Right x -> do
                  modifyIORef' cacheRef (Map.insert cabalPath x)
                  return x
            else return Map.empty
        Just x -> return x

-- | A wrapper around 'parseFixityDeclaration' for parsing individual fixity
-- definitions.
parseFixityDeclarationStr ::
  -- | Input to parse
  String ->
  -- | Parse result
  Either String [(String, FixityInfo)]
parseFixityDeclarationStr =
  first errorBundlePretty . parseFixityDeclaration . T.pack

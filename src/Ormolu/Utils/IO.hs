{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Utils.IO
  ( findClosestFileSatisfying,
    Cache,
    newCache,
    withCache,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M
import System.Directory
import System.FilePath
import System.IO.Error (isDoesNotExistError, isPermissionError)

-- | Find the path to the closest file higher in the file hierarchy that
-- satisfies a given predicate.
findClosestFileSatisfying ::
  (MonadIO m) =>
  -- | The predicate that determines what we are looking for
  (FilePath -> Bool) ->
  -- | Path to the starting point for the search
  FilePath ->
  -- | Absolute path to the found file if available
  m (Maybe FilePath)
findClosestFileSatisfying isRightFile rootOfSearch = liftIO $ do
  parentDir <- takeDirectory <$> makeAbsolute rootOfSearch
  maybeDirEntries <-
    (Just <$> listDirectory parentDir) `catch` \case
      -- The directory does not exist. This is expected: the search may start
      -- from a path that does not exist yet (e.g. a file about to be created),
      -- whose absolute form still lies below existing parent directories.
      -- Treat it as empty and keep searching upwards.
      (isDoesNotExistError -> True) -> pure (Just [])
      -- We lack the permissions to read the directory, e.g. when running in a
      -- sandbox that restricts access to parent directories. Abort the search:
      -- we almost certainly cannot read any parent directory either.
      (isPermissionError -> True) -> pure Nothing
      e -> throwIO e
  case maybeDirEntries of
    Nothing -> pure Nothing
    Just entries -> do
      let searchAtParentDirLevel = \case
            [] -> pure Nothing
            x : xs ->
              if isRightFile x
                then
                  doesFileExist (parentDir </> x) >>= \case
                    True -> pure (Just x)
                    False -> searchAtParentDirLevel xs
                else searchAtParentDirLevel xs
      searchAtParentDirLevel entries >>= \case
        Just foundFile -> pure . Just $ parentDir </> foundFile
        Nothing ->
          if isDrive parentDir
            then pure Nothing
            else findClosestFileSatisfying isRightFile parentDir

newtype Cache k v = Cache (IORef (Map k v))

newCache :: (Ord k) => IO (Cache k v)
newCache = do
  var <- newIORef mempty
  pure (Cache var)

-- | Execute an 'IO' action but only if the given key is not found in the
-- cache.
withCache :: (Ord k) => Cache k v -> k -> IO v -> IO v
withCache (Cache cacheVar) k action = do
  cache <- readIORef cacheVar
  case M.lookup k cache of
    Just v -> pure v
    Nothing -> do
      v <- action
      atomicModifyIORef cacheVar ((,()) . M.insert k v)
      pure v

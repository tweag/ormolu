{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Write 'Text' to files using UTF8 and ignoring native
-- line ending conventions.
module Ormolu.Utils.IO
  ( writeFileUtf8,
    readFileUtf8,
    getContentsUtf8,
    findClosestFileSatisfying,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import System.Directory
import System.FilePath
import System.IO.Error (isDoesNotExistError)

-- | Write a 'Text' to a file using UTF8 and ignoring native
-- line ending conventions.
writeFileUtf8 :: (MonadIO m) => FilePath -> Text -> m ()
writeFileUtf8 p = liftIO . B.writeFile p . TE.encodeUtf8

-- | Read an entire file strictly into a 'Text' using UTF8 and
-- ignoring native line ending conventions.
readFileUtf8 :: (MonadIO m) => FilePath -> m Text
readFileUtf8 p = liftIO (B.readFile p) >>= decodeUtf8

-- | Read stdin as UTF8-encoded 'Text' value.
getContentsUtf8 :: (MonadIO m) => m Text
getContentsUtf8 = liftIO B.getContents >>= decodeUtf8

-- | A helper function for decoding a strict 'ByteString' into 'Text'. It is
-- strict and fails immediately if decoding encounters a problem.
decodeUtf8 :: (MonadIO m) => ByteString -> m Text
decodeUtf8 = liftIO . either throwIO pure . TE.decodeUtf8'

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
  dirEntries <-
    listDirectory parentDir `catch` \case
      (isDoesNotExistError -> True) -> pure []
      e -> throwIO e
  let searchAtParentDirLevel = \case
        [] -> pure Nothing
        x : xs ->
          if isRightFile x
            then
              doesFileExist (parentDir </> x) >>= \case
                True -> pure (Just x)
                False -> searchAtParentDirLevel xs
            else searchAtParentDirLevel xs
  searchAtParentDirLevel dirEntries >>= \case
    Just foundFile -> pure . Just $ parentDir </> foundFile
    Nothing ->
      if isDrive parentDir
        then pure Nothing
        else findClosestFileSatisfying isRightFile parentDir

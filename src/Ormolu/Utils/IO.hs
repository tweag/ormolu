-- | Write 'Text' to files using UTF8 and ignoring native
-- line ending conventions.
module Ormolu.Utils.IO
  ( writeFileUtf8,
    readFileUtf8,
    getContentsUtf8,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Write a 'Text' to a file using UTF8 and ignoring native
-- line ending conventions.
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 p = liftIO . B.writeFile p . TE.encodeUtf8

-- | Read an entire file strictly into a 'Text' using UTF8 and
-- ignoring native line ending conventions.
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 p = liftIO (B.readFile p) >>= decodeUtf8

-- | Read stdin as UTF8-encoded 'Text' value.
getContentsUtf8 :: MonadIO m => m Text
getContentsUtf8 = liftIO B.getContents >>= decodeUtf8

-- | A helper function for decoding a strict 'ByteString' into 'Text'. It is
-- strict and fails immediately if decoding encounters a problem.
decodeUtf8 :: MonadIO m => ByteString -> m Text
decodeUtf8 = liftIO . either throwIO pure . TE.decodeUtf8'

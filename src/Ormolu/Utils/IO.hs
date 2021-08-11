-- | Write 'Text' to files using UTF8 and ignoring native
-- line ending conventions.
module Ormolu.Utils.IO
  ( writeFileUtf8,
    readFileUtf8,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
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
readFileUtf8 p = liftIO $ either throwIO pure . TE.decodeUtf8' =<< B.readFile p

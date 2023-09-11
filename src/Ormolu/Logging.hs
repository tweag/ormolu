module Ormolu.Logging
  ( logDebug,
    logError,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Ormolu.Config (Config (..))
import System.IO (hPutStrLn, stderr)

logDebug ::
  (MonadIO m) =>
  Config region ->
  -- | Some label to prefix the message with
  String ->
  -- | The message, ideally on a single line
  String ->
  m ()
logDebug Config {cfgDebug} label msg =
  when cfgDebug $
    logToStderr . unwords $
      [ "*** " <> label <> " ***",
        msg
      ]

logError :: (MonadIO m) => String -> m ()
logError = logToStderr

logToStderr :: (MonadIO m) => String -> m ()
logToStderr = liftIO . hPutStrLn stderr

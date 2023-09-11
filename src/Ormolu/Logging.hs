module Ormolu.Logging
  ( initializeLogging,
    logDebug,
    logDebugM,
    logError,
    logErrorM,
  )
where

import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Ormolu.Config (Config (..))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

data LoggerConfig = LoggerConfig
  { debugEnabled :: Bool
  }

loggerConfig :: IORef LoggerConfig
loggerConfig = unsafePerformIO $ newIORef (error "Logger not configured yet")
{-# NOINLINE loggerConfig #-}

initializeLogging :: Config region -> IO ()
initializeLogging cfg =
  writeIORef loggerConfig $
    LoggerConfig
      { debugEnabled = cfgDebug cfg
      }

-- | Output a debug log to stderr.
--
-- Requires initializeLogging to be called first.
logDebug ::
  -- | Some label to prefix the message with
  String ->
  -- | The message, ideally on a single line
  String ->
  a ->
  a
logDebug label msg = logToStderr getMessage
  where
    getMessage = do
      cfg <- readIORef loggerConfig
      pure $
        if debugEnabled cfg
          then
            Just . unwords $
              [ "*** " <> label <> " ***",
                msg
              ]
          else Nothing

-- | Output a debug log to stderr.
--
-- Requires initializeLogging to be called first.
logDebugM ::
  (Monad m) =>
  -- | Some label to prefix the message with
  String ->
  -- | The message, ideally on a single line
  String ->
  m ()
logDebugM label msg = logDebug label msg $ pure ()

logError :: String -> a -> a
logError = logToStderr . pure . Just

logErrorM :: (Monad m) => String -> m ()
logErrorM msg = logError msg $ pure ()

logToStderr :: IO (Maybe String) -> a -> a
logToStderr getMessage a =
  unsafePerformIO $ do
    traverse_ (hPutStrLn stderr) =<< getMessage
    pure a

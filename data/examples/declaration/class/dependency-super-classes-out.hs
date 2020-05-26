{-# LANGUAGE FunctionalDependencies #-}

module Main where

-- | Something.
class (MonadReader r s, MonadWriter w m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

-- | 'MonadParsec'
class
  ( Stream s -- Token streams
  , MonadPlus m -- Potential for failure
  ) =>
  MonadParsec e s m
    | m -> e s
  where
  -- | 'getState' returns state
  getState ::
    m s

  -- | 'putState' sets state
  putState ::
    s ->
    m ()

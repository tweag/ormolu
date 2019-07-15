newtype R r a = R (ReaderT r IO a)
  deriving (MonadReader r)

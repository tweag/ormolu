instance MonadReader a ((->) a) where ask = id

instance MonadState s (State s) where
  get = State.get
  put = State.put

data TBQueue a
  = TBQueue
      {-# UNPACK #-} !(TVar Natural) -- CR:  read capacity
      {-# UNPACK #-} !(TVar [a]) -- R:   elements waiting to be read

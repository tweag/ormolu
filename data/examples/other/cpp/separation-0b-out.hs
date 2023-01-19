{-# LANGUAGE CPP #-}

instance (Stream s) => Monad (ParsecT e s m) where
  return = pure
  (>>=) = pBind
#if !(MIN_VERSION_base(4,13,0))
  fail   = Fail.fail
#endif

foo :: Int
foo = undefined

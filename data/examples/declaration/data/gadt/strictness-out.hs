data Foo a where
  Foo1 :: !Int -> {-# UNPACK #-} !Bool -> Foo Int
  Foo2 :: {-# UNPACK #-} Maybe Int && Bool -> Foo Int

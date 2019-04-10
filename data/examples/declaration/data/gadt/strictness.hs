data Foo a where
  Foo :: !Int -> {-# UNPACK #-} !Bool -> Foo Int

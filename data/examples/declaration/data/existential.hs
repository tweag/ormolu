{-# LANGUAGE ExistentialQuantification #-}

data Foo = forall a. MkFoo a (a -> Bool)

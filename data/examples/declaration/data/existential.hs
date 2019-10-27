{-# LANGUAGE ExistentialQuantification #-}

data Foo = forall a. MkFoo a (a -> Bool)

data Bar = forall a b. a + b => Bar a b

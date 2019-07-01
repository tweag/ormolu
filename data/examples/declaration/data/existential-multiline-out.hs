{-# LANGUAGE ExistentialQuantification #-}

data Foo
  = forall a. MkFoo a (a -> Bool)
  | forall a. Eq a => MkBar a

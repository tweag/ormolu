{-# LANGUAGE ExistentialQuantification #-}

data Foo
  = forall a. MkFoo a (a -> Bool)
  | forall a. (Eq a) => MkBar a

data Bar
  = forall x y.
    Bar x y x y

data Baz
  = forall x y.
    Baz
      x
      y
      x
      y

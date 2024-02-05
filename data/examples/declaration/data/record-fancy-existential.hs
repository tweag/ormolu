{-# LANGUAGE ExistentialQuantification #-}
data Foo = forall a b . (Show a, Eq b) => Bar
  { foo  :: a
  , bars :: b
  }

module Main where

-- | Foo
class Foo a where
  foo :: a -> a
  foo a = a

-- | Bar
class Bar a where
  bar
    :: a
    -> Int
  bar = const 0

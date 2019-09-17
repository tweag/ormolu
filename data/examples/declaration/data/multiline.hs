module Main where

-- | Here we have 'Foo'.

data Foo
  = Foo -- ^ One
  | Bar Int -- ^ Two
  | Baz -- ^ Three
  deriving
    (Eq, Show)

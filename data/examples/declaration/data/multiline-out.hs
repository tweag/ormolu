module Main where

-- | Here we have 'Foo'.
data Foo
  = -- | One
    Foo
  | -- | Two
    Bar Int
  | -- | Three
    Baz
  deriving
    (Eq, Show)

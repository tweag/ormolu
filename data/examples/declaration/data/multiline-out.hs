module Main where

-- | Here we have 'Foo'.
data Foo
  = Foo -- ^ One
  | -- | Two
    Bar Int
  | Baz -- ^ Three
  | -- | Four
    -- more about four
    Qux
  deriving
    (Eq, Show)

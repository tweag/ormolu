module Main where

-- | Something.
class Foo a where
  -- | Foo
  foo :: a -> String
  default foo :: Show a => a -> String
  foo = show

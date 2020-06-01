module Main where

-- | Foo.
data Foo a b where
  -- | Something
  Foo :: Foo Int Int
  -- | Something else
  Bar
    :: Foo
         Char
         Char

module Main where

-- | Something.
data Foo
  = Foo
      Bar
      (Set Baz) -- and here we go
      -- and that's it
      Text
  deriving (Eq)

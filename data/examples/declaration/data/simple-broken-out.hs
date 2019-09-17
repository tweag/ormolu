module Main where

-- | Here we go.
data Foo
  = Foo {unFoo :: Int}
  deriving (Eq)

-- | And once again.
data Bar = Bar {unBar :: Int}
  deriving (Eq)

module Main where

-- | Something.
class Foo a where foo :: a

-- | Something more.
class Bar a where
  -- | Bar
  bar :: a -> a -> a

class Baz a where
  -- | Baz
  baz ::
    -- | First argument
    ( a,
      a
    ) ->
    -- | Second argument
    a ->
    -- | Return value
    a

class BarBaz a where
  barbaz ::
    a -> b
  bazbar :: b -> a

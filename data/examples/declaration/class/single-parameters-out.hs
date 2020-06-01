module Main where

-- | Something.
class Foo a where foo :: a

-- | Something more.
class Bar a where
  -- | Bar
  bar :: a -> a -> a

class Baz a where
  -- | Baz
  baz
    :: ( a
       , a
       ) -- ^ First argument
    -> a -- ^ Second argument
    -> a -- ^ Return value

class BarBaz a where
  barbaz
    :: a -> b
  bazbar :: b -> a

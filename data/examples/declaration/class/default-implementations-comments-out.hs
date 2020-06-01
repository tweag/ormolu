module Main where

-- | Baz
class Baz a where
  foobar :: a -> a
  foobar a =
    barbaz (bazbar a)

  -- | Bar baz
  barbaz
    :: a -> a

  -- | Baz bar
  bazbar
    :: a
    -> a

  -- First comment
  barbaz a =
    bazbar -- Middle comment
      a

  -- Last comment
  bazbar a =
    barbaz
      a

{-# LANGUAGE FunctionalDependencies #-}

module Main where

-- | Something.
class Foo a b | a -> b

class Bar a b | a -> b, b -> a where bar :: a

-- | Something else.
class
  Baz a b c d
    | a b -> c d -- Foo
    , b c -> a d -- Bar
    , a c -> b d -- Baz
    , a c d -> b
    , a b d -> a b c d
  where
  baz :: a -> b

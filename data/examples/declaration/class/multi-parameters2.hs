module Main where

{-# LANGUAGE MultiParamTypeClasses #-}

-- | Something else.
class
      BarBaz
        a              -- Foo
        b              -- Bar
        c              -- Baz bar
        d              -- Baz baz
        e              -- Rest
        f where
    barbaz ::
        a -> f
    bazbar ::
        e ->
        f

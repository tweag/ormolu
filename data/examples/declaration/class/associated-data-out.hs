{-# LANGUAGE TypeFamilies #-}

class Foo a where data FooBar a

-- | Something.
class Bar a where

  -- | Bar bar
  data BarBar a

  -- | Bar baz
  data
    BarBaz
      a

-- | Something more.
class Baz a where

  -- | Baz bar
  data
    BazBar
      a
      b
      c

  -- | Baz baz
  data
    BazBaz
      b
      a
      c

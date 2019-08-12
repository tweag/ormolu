{-# LANGUAGE TypeFamilies #-}

class Foo a where type FooBar a

-- | Something.
class Bar a where

  -- | Bar bar
  type BarBar a

  -- | Bar baz
  type
    BarBaz
      a

-- | Something more.
class Baz a where

  -- | Baz bar
  type
    BazBar
      a -- Foo
      b -- Bar
      c

  -- | Baz baz
  type
    -- After type
    BazBaz
      b
      a
      c

{-# LANGUAGE TypeFamilies #-}
class Foo a where
  type FooBar a

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
  type BazBar a b c
  -- | Baz baz
  type
    BazBaz
      b
      a
      c

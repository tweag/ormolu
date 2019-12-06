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

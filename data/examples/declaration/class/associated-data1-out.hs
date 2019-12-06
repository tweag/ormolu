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

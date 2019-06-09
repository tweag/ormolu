{-# LANGUAGE TypeFamilies #-}

class Foo a where type FooBar a = Int

-- | Something.
class Bar a where
  -- Define bar
  type BarBar a
    = BarBaz a
  -- Define baz
  type BarBaz
         a = BarBar -- Middle comment
          a

class Baz a where
  type BazBar
         a

  type BazBar a =
         Bar a

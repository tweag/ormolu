{-# LANGUAGE TypeFamilies #-}

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

{-# LANGUAGE TypeFamilies #-}

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

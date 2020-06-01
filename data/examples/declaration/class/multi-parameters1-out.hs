{-# LANGUAGE MultiParamTypeClasses #-}

class Foo a b where foo :: a -> b

-- | Something.
class Bar a b c d where
  bar
    :: a
    -> b
    -> c
    -> d

class -- Before name
  Baz where
  baz :: Int

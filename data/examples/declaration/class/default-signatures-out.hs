{-# LANGUAGE DefaultSignatures #-}

-- | Something.
class Foo a where

  -- | Foo
  foo :: a -> String

  default foo :: Show a => a -> String
  foo = show

-- | Something else.
class Bar a where

  -- | Bar
  bar
    :: String
    -> String
    -> a

  -- Pointless comment
  default bar
    :: ( Read a
       , Semigroup a
       )
    => a
    -> a
    -> a
  -- Even more pointless comment
  bar
    a
    b =
      read a <> read b

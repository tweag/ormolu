{-# LANGUAGE ExplicitForAll #-}

module Main where

-- | Here goes a comment.
data Foo a where
  -- | 'Foo' is wonderful.
  Foo :: forall a b. (Show a, Eq b) => a -> b -> Foo 'Int
  Bar ::
    Int ->
    Text ->
    -- | But 'Bar' is also not too bad.
    Foo 'Bool
  Baz ::
    forall a.
    a ->
    -- | So is 'Baz'.
    Foo 'String

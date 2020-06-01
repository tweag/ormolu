{-# LANGUAGE ExplicitForAll #-}

module Main where

-- | Here goes a comment.
data Foo a where
  -- | 'Foo' is wonderful.
  Foo :: forall a b. (Show a, Eq b) => a -> b -> Foo 'Int
  Bar
    :: Int
    -> Text
    -> Foo 'Bool -- ^ But 'Bar' is also not too bad.
  Baz
    :: forall a.
    a
    -> Foo 'String -- ^ So is 'Baz'.

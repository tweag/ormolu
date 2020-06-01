{-# LANGUAGE ExplicitForAll #-}

module Main where

-- | Here goes a comment.
data Foo a where
  -- | 'Foo' is wonderful.
  Foo
    :: forall a b.
    (Show a, Eq b) => -- foo
    -- bar
    a
    -> b
    -> Foo 'Int
  -- | But 'Bar' is also not too bad.
  Bar
    :: Int
    -> Maybe Text
    -> Foo 'Bool
  -- | So is 'Baz'.
  Baz
    :: forall a.
    a
    -> Foo 'String
  (:~>) :: Foo a -> Foo a -> Foo a

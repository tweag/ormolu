module Main where

-- | Something.
data Foo where
  Foo :: {fooX :: Int} -> Foo
  Bar ::
    { fooY :: Int
    , fooBar, fooBaz :: Bool
    , fooFoo
      , barBar
      , bazBaz ::
        Int
    } ->
    Foo

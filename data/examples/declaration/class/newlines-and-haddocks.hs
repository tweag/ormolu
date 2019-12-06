class Foo a where
  -- | Haddock
  foo :: a
  -- | Another Haddock
  bar :: a
  baz :: a
  -- ^ Post-Haddock
  raz :: a
  -- ^ Another Post-Haddock

  -- | One more Haddock
  qux :: a
  -- Comment before a Haddock
  -- | And one more Haddock
  xyz :: a

  -- | Haddock followed by a blank line

  abc :: a

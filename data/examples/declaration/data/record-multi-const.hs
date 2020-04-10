module Main where

-- | Something.

data Foo = Foo
  { fooX :: Int -- ^ X
  , fooY :: Int -- ^ Y
  } |
  Bar
  { barX :: Int -- ^ X
  , barY :: Int -- ^ Y
  }
  deriving (Eq, Show)

module Main where

-- | Something.
data Foo
  = Foo
      { -- | X
        fooX :: Int
      , -- | Y
        fooY :: Int
      }
  | Bar
      { -- | X
        barX :: Int
      , -- | Y
        barY :: Int
      }
  deriving (Eq, Show)

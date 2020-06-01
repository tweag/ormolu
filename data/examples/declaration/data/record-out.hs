module Main where

-- | Something.
data Foo = Foo
  { -- | X
    fooX :: Int
  , -- | Y
    fooY :: Int
  , -- | BarBaz
    fooBar, fooBaz :: NonEmpty (Identity Bool)
  , -- | GagGog
    fooGag
    , fooGog
    :: NonEmpty
           ( Indentity
               Bool
           )
  , -- | Huh!
    fooFoo
    , barBar
    :: Int
  }
  deriving (Eq, Show)

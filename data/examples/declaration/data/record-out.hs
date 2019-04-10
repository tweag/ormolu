-- | Something.
data Foo
  = Foo
      { fooX :: Int -- ^ X
      , fooY :: Int -- ^ Y
      , fooBar, fooBaz :: Bool -- ^ BarBaz
      , fooFoo
        , barBar
          :: Int -- ^ Huh!
      }
  deriving (Eq, Show)

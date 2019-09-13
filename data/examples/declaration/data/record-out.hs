-- | Something.

data Foo
  = Foo
      { fooX :: Int, -- ^ X
        fooY :: Int, -- ^ Y
        fooBar, fooBaz :: NonEmpty (Identity Bool), -- ^ BarBaz
        fooGag,
        fooGog
          :: NonEmpty
               ( Indentity
                   Bool
                 ),
        -- ^ GagGog
        fooFoo,
        barBar
          :: Int -- ^ Huh!
        }
  deriving (Eq, Show)

module Main where

-- | Something.

data Foo = Foo
  { fooX :: Int -- ^ X
  , fooY :: Int -- ^ Y
  , fooBar, fooBaz :: NonEmpty (Identity Bool) -- ^ BarBaz
  , fooGag, fooGog :: NonEmpty (Identity
                                  Bool)
    -- ^ GagGog
  , fooFoo
      , barBar :: Int -- ^ Huh!
  } deriving (Eq, Show)

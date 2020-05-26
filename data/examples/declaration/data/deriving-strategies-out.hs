module Main where

-- | Something.
newtype Foo = Foo Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass
    ( ToJSON
    , FromJSON
    )
  deriving newtype (Num)
  deriving (Monoid) via (Sum Int)
  deriving
    (Semigroup)
    via (Sum Int)

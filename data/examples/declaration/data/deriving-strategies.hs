-- | Something.

newtype Foo = Foo Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass
    ( ToJSON
    , FromJSON
    )
  deriving newtype (Num)

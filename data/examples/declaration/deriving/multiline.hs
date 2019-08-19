deriving instance Eq
                    Foo

deriving stock instance
  Show
    Foo
deriving anyclass instance
  ToJSON
    Foo
deriving newtype instance
  Data
    Foo

deriving via Foo
               Int
  instance Triple
             A
             B
             C

deriving instance
  {-# OVERLAPPABLE #-}
  Ord
    Foo

deriving instance
  {-# OVERLAPPING #-}
  Num
    Foo

deriving instance
  {-# OVERLAPS #-}
  Read
    Foo

deriving instance
  {-# INCOHERENT #-}
  Show
    Foo

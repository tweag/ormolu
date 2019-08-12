instance {-# OVERLAPPABLE #-} Eq Int where (==) _ _ = False

instance {-# OVERLAPPING #-} Ord Int where

  compare _ _ = GT

instance {-# OVERLAPS #-} Eq Double where

  (==) _ _ = False

instance
  {-# INCOHERENT #-}
  Ord
    Double
  where

  compare _ _ = GT

{-# LANGUAGE InstanceSigs #-}

instance Eq Int where
  (==) :: Int -> Int -> Bool
  (==) _ _ = False

instance Ord Int where
  compare
    :: Int
    -> Int
    -> Ordering
  compare
    _
    _ =
      GT

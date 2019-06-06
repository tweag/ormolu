instance Eq a => Eq [a] where
  (==) _ _ = False

instance ( Ord a
         , Ord b
         )
         => Ord
              (a, b) where
  compare _ _ = GT

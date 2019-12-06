data Foo a = Foo a

data VT v m r = VT v m r

instance (Eq a) => Eq (Foo a) where
  {-# SPECIALIZE instance Eq (Foo [(Int, Bar)]) #-}
  (==) (Foo a) (Foo b) = (==) a b

instance (Num r, V.Vector v r, Factored m r) => Num (VT v m r) where
  {-# SPECIALIZE instance
    ( Factored m Int => Num (VT U.Vector m Int)
    )
    #-}
  VT x + VT y = VT $ V.zipWith (+) x y

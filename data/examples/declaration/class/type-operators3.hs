{-# LANGUAGE TypeFamilies, TypeOperators, NoStarIsType, PolyKinds #-}

class PNum x where
  type (a :: x) * (b :: x)

instance PNum Nat where
  type a * b = ()

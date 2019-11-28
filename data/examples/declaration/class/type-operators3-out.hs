{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

class PNum x where
  type (a :: x) * (b :: x)

instance PNum Nat where
  type a * b = ()

{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

class PNum x where
  type (a :: x) * (b :: x)

instance PNum Nat where
  type a * b = ()

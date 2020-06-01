{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- Multiline foralls are consistent across all declarations

data D
  = forall
      ( f
        :: * -> * -> *
      )
      (x :: *)
      (y :: *).
    D (f x y)

data G where
  G
    :: forall
      ( f
        :: * -> * -> *
      )
      (x :: *)
      (y :: *).
    f x y
    -> G

f
  :: forall
    ( f
      :: * -> * -> *
    )
    (x :: *)
    (y :: *).
     f x y
  -> ()
f = const ()

type family T f x y where
  forall
    ( f
      :: * -> * -> *
    )
    (x :: *)
    (y :: *).
    T f x y =
      f x y

{-# RULES
"r" forall
  ( f
      :: * -> * -> *
  )
  (x :: *)
  (y :: *).
  r (a :: f x y) =
    ()
  #-}

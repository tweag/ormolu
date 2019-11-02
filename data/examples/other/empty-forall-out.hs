-- Empty foralls are handled correctly in different situations.

data D = forall. D Int

data G where
  G :: forall. Int -> G

f :: forall. a -> a
f x = x

type family T x where
  forall. T x = x

{-# RULES
"r"
  r a =
    ()
  #-}

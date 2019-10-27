type family G a b where
  forall x y. G [x] (Proxy y) = Double
  forall z. z `G` z = Bool

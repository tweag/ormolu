data T a where
  Typed :: forall a -> a -> T a

f1 (Typed a x) = x :: a

f2 (Typed Int n) = n * 2

f3 (Typed ((->) w Bool) g) = not . g

data D x where
  MkD1 ::
    forall a b ->
    a ->
    b ->
    D (a, b)
  MkD2 ::
    forall a.
    forall b ->
    a ->
    b ->
    D (a, b)
  MkD3 ::
    forall a ->
    a ->
    forall b ->
    b ->
    D (a, b)

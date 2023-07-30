type T :: forall k. k -> forall j. j -> Type
data T @k (a :: k) @(j :: Type) (b :: j)

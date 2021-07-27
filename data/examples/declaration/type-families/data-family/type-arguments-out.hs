type PickType :: forall k. (k -> Type) -> Type
data family PickType m

data instance PickType @Nat M where
  Foo :: PickType M

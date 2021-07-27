type PickType :: forall k. Nat -> k
type family PickType n where
  PickType @Type 1 = Bool
  PickType @(Type -> Type) 2 = Maybe

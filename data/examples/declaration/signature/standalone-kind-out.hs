{-# LANGUAGE StandaloneKindSignatures #-}

type T
  :: (k -> Type)
  -> k
  -> Type
data T m a = MkT (m a) (T Maybe (m a))

type C1 :: Type -> Constraint
class C1 a

type F :: Type -> Type
type family F

{-# LANGUAGE UnicodeSyntax #-}

data Foo :: Type -> Type where
  Foo :: a -> Foo a

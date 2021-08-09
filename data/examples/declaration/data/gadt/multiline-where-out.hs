data
  Foo
    a
    b
    c
  where
  Foo :: a -> b -> c -> Foo a b c

data
  Foo ::
    Type ->
    Type ->
    Type
  where
  Foo :: Foo a b

data
  Foo a b c ::
    Type -> Type -> Type -> Type
  where
  Foo :: Foo a b c

data
  Vec ::
    Type ->
    Type ->
    Type
  where
  Nil :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)

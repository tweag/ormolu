data
  Foo
    a
    b
  = Foo a b

data a :-> b = Arrow (a -> b)

data (f :* g) a = f a :* g a

data
  ( f
      :+ g
  )
    a
  = L (f a)
  | R (g a)

data a `Arrow` b = Arrow' (a -> b)

data (f `Product` g) a = f a `Product` g a

data
  ( f
      `Sum` g
  )
    a
  = L' (f a)
  | R' (g a)

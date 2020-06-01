type family
  Id a =
    result | result -> a
  where
  Id a =
    a

type family
  G
    (a :: k)
    b
    c =
    foo | foo -> k b
  where
  G a b c =
    (a, b)

type family
  F a
    :: * -> *
  where
  F Int = Double
  F Bool =
    Char
  F a = String

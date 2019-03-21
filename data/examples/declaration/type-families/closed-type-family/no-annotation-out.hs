type family F a where
  F Int = Double
  F Bool = Char
  F a = String

data IsString s => T s = T

data IsString s =>
  T s = T

data
  ( IsString s
  , IsString s ) =>  T s = T

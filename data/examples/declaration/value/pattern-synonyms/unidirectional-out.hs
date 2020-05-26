{-# LANGUAGE PatternSynonyms #-}

pattern Head x <- x : xs

pattern Head' x <-
  x : xs

pattern Head'' {x} <-
  x : xs

pattern FirstTwo {x, y} <-
  x : (y : xs)

pattern FirstTwo'
  { x
  , y
  } <-
  x : (y : xs)

pattern Simple <- "Simple"

pattern WithTypeSig :: String
pattern WithTypeSig <- "WithTypeSig"

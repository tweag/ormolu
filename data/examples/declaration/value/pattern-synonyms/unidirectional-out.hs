{-# LANGUAGE PatternSynonyms #-}
pattern Head x <- x : xs

pattern Head' x <-
  x : xs

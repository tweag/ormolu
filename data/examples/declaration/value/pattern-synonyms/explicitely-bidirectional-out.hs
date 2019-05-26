{-# LANGUAGE PatternSynonyms #-}
pattern HeadC x <-
  x : xs
  where
    HeadC x = [x]

pattern HeadC' x <-
  x : xs
  where
    HeadC' x = [x]

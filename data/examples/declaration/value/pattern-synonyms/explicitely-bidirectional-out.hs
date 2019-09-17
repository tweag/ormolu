{-# LANGUAGE PatternSynonyms #-}

pattern HeadC x <-
  x : xs
  where
    HeadC x = [x]

pattern HeadC' x <-
  x : xs
  where
    HeadC' x = [x]

pattern Simple <-
  "Simple"
  where
    Simple = "Complicated"

pattern a :< b <-
  (a, b)
  where
    a :< b = (a, b)

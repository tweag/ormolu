{-# LANGUAGE PatternSynonyms #-}

pattern P a <- C a where P a = C a

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

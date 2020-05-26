{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

pattern Arrow t1 t2 = App "->" [t1, t2]

pattern Arrow {t1, t2} = App "->" [t1, t2]

pattern Arrow
  { t1
  , t2
  } =
  App "->" [t1, t2]

pattern Int =
  App "Int" []

pattern Maybe {t} =
  App
    "Maybe"
    [t]

pattern Maybe t =
  App
    "Maybe"
    [t]

pattern a :< b <-
  (a, b)

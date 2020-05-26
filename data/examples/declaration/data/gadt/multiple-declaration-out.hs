data GADT0 a where
  GADT01, GADT02 :: Int -> GADT0 a

data GADT1 a where
  GADT11
    , GADT12 ::
    Int ->
    GADT1 a

data GADT2 a where
  GADT21
    , GADT21
    , GADT22 ::
    Int ->
    GADT2 a

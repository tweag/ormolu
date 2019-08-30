{-# LANGUAGE LambdaCase #-}

foo = bar (\case JKey {} -> True; _ -> False)

foo :: Int -> Int
foo = \case
  5 -> 10
  i | i > 5 -> 11
  _ -> 12

{-# LANGUAGE LambdaCase #-}

foo :: Int -> Int
foo = \case
  5 -> 10
  i  | i > 5 -> 11
  _ -> 12

{-# LANGUAGE LambdaCase #-}
foo :: Int -> Int
foo = \case
  5 -> 10
  _ -> 12

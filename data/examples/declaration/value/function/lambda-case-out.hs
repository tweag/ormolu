{-# LANGUAGE LambdaCase #-}

foo = bar (\case JKey {} -> True; _ -> False)

foo :: Int -> Int
foo = \case
  5 -> 10
  i | i > 5 -> 11
  _ -> 12

foo :: Maybe a -> Maybe a -> Maybe a -> Int
foo = \cases Nothing Just {} Nothing -> 1; _ _ _ -> 0

foo :: Maybe Int -> Maybe Int -> Int
foo = \cases
  (Just a) (Just a) -> a + a
  _ _ -> 0

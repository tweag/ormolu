{-# LANGUAGE ImplicitParams #-}

sortBy :: (a -> a -> Bool) -> [a] -> [a]

sort :: (?cmp :: a -> a -> Bool) => [a] -> [a]
sort = sortBy ?cmp

sort' ::
  ( ?cmp ::
      a -> a -> Bool
  , ?foo :: Int
  ) =>
  [a] ->
  [a]
sort' = sort

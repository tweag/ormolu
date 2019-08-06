foo :: a -> a -> a
foo x = \y ->
  x

bar :: Int -> Int -> Int
bar x = \y ->
  if x > y
  then 10
  else 12

tricky0 =
  flip all (zip ws gs) $ \(wt, gt) ->
    canUnify poly_given_ok wt gt || go False wt gt

tricky1 =
  flip all (zip ws gs)
    $ \(wt, gt) -> canUnify poly_given_ok wt gt || go False wt gt

tricky2 =
  flip all (zip ws gs)
    $ \(wt, gt) ->
      canUnify poly_given_ok wt gt || go False wt gt

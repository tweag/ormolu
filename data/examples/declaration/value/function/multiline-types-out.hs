foo ::
  (Monad m, Show a) =>
  a ->
  m String
bar ::
  ( Monad m,
    Show a
  ) =>
  a -> m String
multiConstraints ::
  (Show a) =>
  a ->
  a ->
  a ->
  (Eq a) =>
  (Num a) =>
  a -> a -> ()

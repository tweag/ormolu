main = do
  x ::
    Int ->
    Double
    <- pure fromIntegral
  y
    :: Int
    <- foo
  u :: Int
    <- foo
  z ::
    Int <-
    foo
  w :: Int <- do
    foo
  (a, b)
    :: (Int, Int)
    <- do
      foo
  q
    :: Int
    -- comment
    <- foo

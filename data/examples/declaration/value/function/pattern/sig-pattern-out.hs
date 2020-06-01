f = do
  x :: a <- g

f = do
  (x, y)
    :: (a, b) <-
    g

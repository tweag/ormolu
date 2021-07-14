main = case [1] of
  xs@(x:_) -> print (x, xs)
  xs@[] -> print xs

data X = X {x :: Int}

f =
  id
    . (\s -> s {x = 1}) -- Some comment

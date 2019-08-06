x `op1` (Just 0) = True
op1 x (Just _) = False
op1 x Nothing = undefined

op2 1 y = False
x `op2` y =
  True

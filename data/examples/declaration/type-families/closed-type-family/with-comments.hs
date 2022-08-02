type family LT a b where
  -- 0
  LT 0 _ = True

  -- 1
  LT 1 0 = False
  LT 1 _ = True

  -- 2
  LT 2 0 = False
  LT 2 1 = False
  LT 2 _ = True

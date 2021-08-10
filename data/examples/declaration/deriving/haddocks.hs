data A = A
  -- | B
  deriving (Eq)

data B = B
  deriving ({- | test -} Eq)

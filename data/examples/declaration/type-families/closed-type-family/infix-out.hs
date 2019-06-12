type family (x :: N) + (y :: N) :: N where
  'Zero + y = y
  'Succ n + y = 'Succ (n + y)

type family (x :: N) `LEQ` (y :: N) :: Bool where
  'Zero `LEQ` y = 'True
  'Succ n `LEQ` 'Zero = 'False
  'Succ n `LEQ` 'Succ m = n `LEQ` m

type family ((x :: N) `Weird` (y :: N)) (z :: N) :: Bool where
  'Zero `Weird` 'Zero 'Zero = 'True

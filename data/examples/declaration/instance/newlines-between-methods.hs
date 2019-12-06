instance Num a => Num (Diff a) where
  D u dudx + D v dvdx = D (u + v) (dudx + dvdx)
  D u dudx - D v dvdx = D (u - v) (dudx - dvdx)
  D u dudx * D v dvdx = D (u * v) (u * dvdx + v * dudx)

  -- Comment before definition
  negate (D u dudx) = D (-u) (-dudx)
  negate (Z u dudx) = undefined
  -- Comment after definition

  -- Separator

  abs (D u _) = D (abs u) (signum u)
  signum (D u _) = D (signum u) 0
  -- Comment between unrelated definitions
  fromInteger n = D (fromInteger n) 0

instance Num a => Num (Diff a) where

  D u dudx + D v dvdx = D (u + v) (dudx + dvdx)
  D u dudx - D v dvdx = D (u - v) (dudx - dvdx)
  D u dudx * D v dvdx = D (u * v) (u * dvdx + v * dudx)

  negate (D u dudx) = D (-u) (-dudx)
  negate (Z u dudx) = undefined


  abs (D u _) = D (abs u) (signum u)
  signum (D u _) = D (signum u) 0
  fromInteger n = D (fromInteger n) 0

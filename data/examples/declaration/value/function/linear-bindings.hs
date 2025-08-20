{-# Language LinearTypes #-}

h x = g y
  where
    %1 y = f x

let %1 x = u in ()
let %Many (x, y) = u in ()
let %1 ~(x, y) = u in ()

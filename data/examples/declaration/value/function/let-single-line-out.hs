foo :: a -> a
foo x = let x = x in x
foo x = let x = z where z = 2 in x
foo x = let x = z where { z = 2 }; a = 3 in x
foo x = let g :: Int -> Int; g = id in ()

let a = b; c = do { foo; bar }; d = baz in b
let a = case True of { True -> foo; False -> bar }; b = foo a in b

foo x = let ?g = id; ?f = g in x

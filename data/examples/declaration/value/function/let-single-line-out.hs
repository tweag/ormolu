foo :: a -> a
foo x = let x = x in x
foo x = let x = z where z = 2 in x

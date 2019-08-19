foo x y = [(a, b) | a <- x | b <- y]

bar x y z w = [(a, b, c, d) | a <- x, b <- y, a `mod` b == 0 | c <- z | d <- w]

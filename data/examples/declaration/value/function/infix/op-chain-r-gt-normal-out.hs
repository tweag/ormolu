-- Right chain, normal case, 2 operators with p(a) > p(b)
f :: Int
f =
  1
    ^^ 2
    ^^ 3
    ^^ 4
    ++ 5
      ^^ 6
      ^^ 7
      ^^ 8

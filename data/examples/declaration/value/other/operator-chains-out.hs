module A (a) where

-- Template
-- x :: Int
-- x =
--   1 op_a
--   2 op_a
--   3 op_a
--   4 op_b
--   5 op_a
--   6 op_a
--   7 op_a
--   8

-- Right chain, normal case, 1 operator type
a :: Int
a =
  1
  ++ 2
  ++ 3
  ++ 4

-- Right chain, : case, 1 operator type
b :: Int
b =
  1
  : 2
  : 3
  : 4
  : 5
  : 6
  : 7
  : 8

-- Right chain, $ case, 1 operator type
c :: Int
c =
  1 $
    2 $
      3 $
        4 $
          5 $
            6 $
              7 $
                8

-- Right chain, normal case, 2 operators with p(a) == p(b)
e :: Int
e =
  1
  ^^ 2
  ^^ 3
  ^^ 4
  ^ 5
  ^^ 6
  ^^ 7
  ^^ 8

-- Right chain, normal case, 2 operators with p(a) > p(b)
f :: Int
f =
    1
    ^^ 2
    ^^ 3
    ^^ 4
  ++
    5
    ^^ 6
    ^^ 7
    ^^ 8

-- Right chain, normal case, 2 operators with p(a) < p(b)
g :: Int
g =
  1
  ++ 2
  ++ 3
  ++
    4
    ^^ 5
  ++ 6
  ++ 7
  ++ 8

-- Right chain, : case, 2 operators with p(:) == p(b)
h :: Int
h =
  1
  : 2
  : 3
  : 4
  ++ 5
  : 6
  : 7
  8

-- Right chain, : case, 2 operators with p(:) > p(b)
i :: Int
i =
    1
    : 2
    : 3
    : 4
  `seq`
    5
    : 6
    : 7
    : 8

-- Right chain, : case, 2 operators with p(:) < p(b)
j :: Int
j =
  1
  : 2
  : 3
  :
    4
    ^^ 5
  : 6
  : 7
  : 8

-- Right chain, : case, 2 operators with p(a) == p(:)
k :: Int
k =
  1
  ++ 2
  ++ 3
  ++ 4
  : 5
  ++ 6
  ++ 7
  ++ 8

-- Right chain, : case, 2 operators with p(a) > p(:)
l :: Int
l =
    1
    ^^ 2
    ^^ 3
    ^^ 4
  :
    5
    ^^ 6
    ^^ 7
    ^^ 8

-- Right chain, : case, 2 operators with p(a) < p(:)
m :: Int
m =
  1
  `seq` 2
  `seq` 3
  `seq`
    4
    : 5
  `seq` 6
  `seq` 7
  `seq` 8

-- Right chain, $ case, 2 operators with p($) == p(b)
n :: Int
n =
  1 $
    2 $
      3 $
        4
        `seq` 5 $
          6 $
            7 $
              8

-- Right chain, $ case, 2 operators with p($) < p(b)
o :: Int
o =
  1 $
    2 $
      3 $
        4
        ++ 5 $
          6 $
            7 $
              8

-- Right chain, $ case, 2 operators with p(a) == p($)
p :: Int
p =
  1
  `seq` 2
  `seq` 3
  `seq` 4 $
    5
    `seq` 6
    `seq` 7
    `seq` 8

-- Right chain, $ case, 2 operators with p(a) > p($)
q :: Int
q =
  1
  ++ 2
  ++ 3
  ++ 4 $
    5
    ++ 6
    ++ 7
    ++ 8

-- Left chain, 1 operator type
r :: Int
r =
  1
  + 2
  + 3
  + 4

-- Left chain, 2 operators with p(a) == p(b)
s :: Int
s =
  1
  + 2
  + 3
  + 4
  - 5
  + 6
  + 7
  + 8

-- Left chain, 2 operators with p(a) > p(b)
t :: Int
t =
    1
    + 2
    + 3
    + 4
  >>
    5
    + 6
    + 7
    + 8

-- Left chain, 2 operators with p(a) < p(b)
u :: Int
u =
  1
  + 2
  + 3
  +
    4
    * 5
  + 6
  + 7
  + 8
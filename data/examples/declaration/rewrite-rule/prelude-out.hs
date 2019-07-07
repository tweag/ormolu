{-# LANGUAGE MagicHash #-}

{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
"map/append" forall f xs ys. map f (xs ++ ys) = map f xs ++ map f ys
  #-}

{-# RULES
"fold/build" forall k z (g :: forall b. (a -> b -> b) -> b -> b).
  foldr k z (build g) =
    g k z
"foldr/augment" forall k z xs (g :: forall b. (a -> b -> b) -> b -> b).
  foldr k z (augment g xs) =
    g k (foldr k z xs)
"foldr/id" foldr (:) [] = \x -> x
"foldr/app" [1] forall ys. foldr (:) ys = \xs -> xs ++ ys
-- Only activate this from phase 1, because that's
-- when we disable the rule that expands (++) into foldr

-- The foldr/cons rule looks nice, but it can give disastrously
-- bloated code when commpiling
--      array (a,b) [(1,2), (2,2), (3,2), ...very long list... ]
-- i.e. when there are very very long literal lists
-- So I've disabled it for now. We could have special cases
-- for short lists, I suppose.
-- "foldr/cons" forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)
"foldr/single" forall k z x. foldr k z [x] = k x z
"foldr/nil" forall k z. foldr k z [] = z
"augment/build" forall
  (g :: forall b. (a -> b -> b) -> b -> b)
  (h :: forall b. (a -> b -> b) -> b -> b).
  augment g (build h) =
    build (\c n -> g c (h c n))
"augment/nil" forall (g :: forall b. (a -> b -> b) -> b -> b).
  augment g [] =
    build g
  #-}

{-# RULES
"map" [~1] forall f xs. map f xs = build (\c n -> foldr (mapFB c f) n xs)
"mapList" [1] forall f. foldr (mapFB (:) f) [] = map f
"mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f . g)
  #-}

{-# RULES
"map/map" [~2] forall f g xs.
  map f (map g xs) =
    map (f . g) xs
"f" op True y = False
"g" op True y = False
  #-}

{-# RULES
"x# `eqChar#` x#" forall x#. x# `eqChar#` x# = True
"x# `neChar#` x#" forall x#. x# `neChar#` x# = False
"x# `gtChar#` x#" forall x#. x# `gtChar#` x# = False
"x# `geChar#` x#" forall x#. x# `geChar#` x# = True
"x# `leChar#` x#" forall x#. x# `leChar#` x# = True
"x# `ltChar#` x#" forall x#. x# `ltChar#` x# = False
  #-}

{-# RULES
"unpack" [~1] forall a. unpackCString# a = build (unpackFoldrCString# a)
"unpack-list" [1] forall a. unpackFoldrCString# a (:) [] = unpackCString# a
"unpack-append" forall a n. unpackFoldrCString# a (:) n = unpackAppendCString# a n
  #-}

-- There's a built-in rule (in PrelRules.lhs) for
--      unpackFoldr "foo" c (unpackFoldr "baz" c n)  =  unpackFoldr "foobaz" c n
{-# RULES
"foldr/build" forall f n (g :: forall b. (a -> b -> b) -> b -> b).
  foldr f n (build g) =
    g f n
  #-}

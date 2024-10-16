{-# LANGUAGE PatternSynonyms #-}

tasty (Cupcake; Cookie) = True
tasty (Liquorice; Raisins) = False

f :: (Eq a, Show a) => a -> a -> Bool
f a ((== a) -> True; show -> "yes") = True
f _ _ = False

small (abs -> (0; 1; 2); 3) = True -- -3 is not small
small _ = False

type Coll a = Either [a] (Set a)

pattern None <- (Left []; Right (toList -> []))

case e of
  1; 2; 3 -> x
  4; (5; 6) -> y

sane e = case e of
  1
  2
  3 -> a
  4
  5
  6 -> b
  7; 8 -> c

insane e = case e of
  A _ _
  B _
  C -> 3
  (D; E (Just _) Nothing) ->
    4
  F -> 5

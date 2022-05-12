testPermParser :: Permutation Parser String
testPermParser =
  f <$> toPermutationWithDefault 'x' (char 'a')
    <*> toPermutationWithDefault 'y' (char 'b')
    <*> toPermutationWithDefault 'z' (char 'c')
  where
    f a b c = [a, b, c]

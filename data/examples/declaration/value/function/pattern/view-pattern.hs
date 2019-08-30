{-# LANGUAGE ViewPatterns #-}

example f (  f    -> 4   ) = True

f (t -> Nothing) = "Nothing"
f (t -> Just _) = "Just"

g ((f, _), f -> 4) = True

multiline (t -> Foo
                  bar
                  baz) = True

-- https://github.com/tweag/ormolu/issues/343
foo = (f -> 4)

{-# LANGUAGE OverloadedLabels #-}

foo = #field

bar = (#this) (#that)

baz = #Foo #"Hello world!" #"\"" #3 #"\n"

-- from https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.6.1-alpha3/testsuite/tests/overloadedrecflds/should_run/T11671_run.hs
-- unnecessary once https://github.com/tweag/ormolu/issues/821 lands
main =
  traverse_
    putStrLn
    [ #a,
      #number17,
      #do,
      #type,
      #Foo,
      #3,
      #"199.4",
      #17a23b,
      #f'a',
      #'a',
      #',
      #''notTHSplice,
      #"...",
      #привет,
      #こんにちは,
      #"3",
      #":",
      #"Foo",
      #"The quick brown fox",
      #"\"",
      (++) #hello #world,
      (++) #"hello" #"world",
      #"hello" # 1, -- equivalent to `(fromLabel @"hello") # 1`
      f "hello" #2 -- equivalent to `f ("hello"# :: Addr#) 2`
    ]

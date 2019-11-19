main =
  do putStrLn "Hi!"
     putStrLn "I am a program!"

foo = 42
  where
    bar =
      do putStrLn "Hi!"
         putStrLn "I am a program!"

bar =
  do putStrLn "Bar!"

baz =
  do
    putStrLn "Baz!"

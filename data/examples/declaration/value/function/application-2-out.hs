{-# LANGUAGE TemplateHaskell #-}

foo = do
  $( bar
   )
    baz

foo = do
  $$( bar
    )
    baz

foo = do
  do (+ 1)
    2

foo = do
  do
      (+ 1)
    2

foo = do
  case () of () -> (+ 1)
    2

foo = do
  case () of
      () -> (+ 1)
    2

foo = do
  \case 2 -> 3
    2

foo = do
  \case
      2 -> 3
    2

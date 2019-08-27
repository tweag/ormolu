foo = bar
  where
    f1 = f1
      where
        f1 = 3
    f2 = f2
      where
        f2 = 3

foo2 = bar
  where
    f1 = f1 where { f1 = 3; f1' = 4 }; f2 = f2 where f2 = 3; f2' = 4

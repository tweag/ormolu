import Database.Esqueleto.Experimental

foo = select $ do
  t <- from $ table @Bar
    `innerJoin` table @Baz
      `on` do
        \(br :& bz) -> whatever
  where_ $
    t ^. BarInt ==. val 3
   &&. t ^. BarName `in_` valList ["hello", "world"]

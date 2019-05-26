pattern Arrow t1 t2 = App "->"    [t1, t2]
pattern Int         =
  App "Int"   []
pattern Maybe t     =
  App
    "Maybe"
    [t]

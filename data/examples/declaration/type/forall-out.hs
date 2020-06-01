type CoerceLocalSig m m' =
  forall r a.
     LocalSig m r a
  -> LocalSig m' r a

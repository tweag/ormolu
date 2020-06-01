{-# RULES "rd_tyvs" forall a. forall (x :: a). id x = x #-}

{-# RULES "rd_tyvs'" forall f a. forall (x :: f a). id x = x #-}

{-# RULES "rd_tyvs''" forall (a :: *). forall (x :: a). id x = x #-}

{-# RULES
"rd_tyvs_multiline1" forall (a :: *). forall (x :: a).
  id x =
    x
  #-}

{-# RULES
"rd_tyvs_multiline2" forall
  ( a
    :: *
  ). forall
  ( x
      :: a
  ).
  id x =
    x
  #-}

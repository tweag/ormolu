{-# LANGUAGE Arrows #-}

foo
  f
  g
  h
  ma =
    proc
      ( (a, b)
        , (c, d)
        , (e, f)
        )
    -> do
      -- Begin do
      (x, y) <- -- GHC parser fails if layed out over multiple lines
        f -- Call into f
          ( a
          , c -- Tuple together arguments
          )
          ( b
          , d
          )
          -<
            ( b + 1 -- Funnel into arrow
            , d * b
            )
      if x `mod` y == 0 -- Basic condition
        then case e of -- Only left case is relevant
          Left
            ( z
              , w
              ) -> \u -> -- Procs can have lambdas
              let v =
                    u -- Actually never used
                      ^ 2
               in (returnA
                     -<
                       -- Just do the calculation
                       (x + y * z))
        else do
          let u = x -- Let bindings bind expressions, not commands
          -- Could pattern match directly on x
          i <- case u of
            0 -> (g . h -< u)
            n ->
              ((h . g
                  -<
                    y)) -- First actual use of y
          returnA -< ()
          -- Sometimes execute effects
          if i > 0
            then ma -< ()
            else returnA -< ()
          returnA
            -<
              (i
                 + x
                 * y) -- Just do the calculation

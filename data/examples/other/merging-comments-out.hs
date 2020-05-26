foo xs = baz
  where
    bar =
      catMaybes
        [ lookup langKey gets -- 1
        , lookup langKey cookies -- 2
        , lookupText langKey session -- 3
        ]
        ++ xs -- 4

    -- Blah
    baz = addTwoLetters (id, Set.empty) bar

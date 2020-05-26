readerBench doc name =
  runPure $ case (getReader name, getWriter name) of
    ( Right (TextReader r, rexts)
      , Right (TextWriter w, wexts)
      ) -> undefined

f xs = case xs of
  [ a
    , b
    ] -> a + b

g xs = case xs of
  ( a
      : bs
    ) -> a + b

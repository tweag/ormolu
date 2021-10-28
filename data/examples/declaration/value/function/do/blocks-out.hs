foo = do bar

foo = do bar; baz

foo = do
  bar
  baz

foo = do do { foo; bar }; baz

readInClause = do
  do
    lookAhead g_Do
    parseNote ErrorC 1063 "You need a line feed or semicolon before the 'do'."
   <|> do
      optional g_Semi
      void allspacing

  return things

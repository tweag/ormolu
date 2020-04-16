main = do
  case blah of
    Nothing -> return ()
    Just xs -> do
      forM_ xs foo
      bar
  -- and here it goes
  unless bobla $
    quux

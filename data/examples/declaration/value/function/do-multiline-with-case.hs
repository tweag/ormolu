handleInput :: IO ()
handleInput = do
  putStrLn "Enter command:"
  cmd <- getLine
  case cmd of
    "quit" -> putStrLn "Goodbye"
    "help" -> do
      putStrLn "Available commands:"
      putStrLn "  quit - exit the program"
      putStrLn "  help - show this message"
    _ -> do
      putStrLn $ "Unknown command: " ++ cmd
      handleInput

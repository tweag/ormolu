processValue :: Maybe Int -> IO ()
processValue x = do putStrLn "Processing:"; \case { Just n -> print n; Nothing -> putStrLn "Empty" } x; putStrLn "Done"

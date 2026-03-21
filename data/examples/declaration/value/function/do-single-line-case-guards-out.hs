checkValue :: Int -> IO ()
checkValue n = do putStr "Value is: "; case () of { _ | n < 0 -> putStrLn "negative" | n == 0 -> putStrLn "zero" | otherwise -> putStrLn "positive" }

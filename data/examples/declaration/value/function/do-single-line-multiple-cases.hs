processPair :: Maybe Int -> Maybe String -> IO ()
processPair x y = do {case x of {Just n -> print n; Nothing -> putStrLn "No number"}; case y of {Just s -> putStrLn s; Nothing -> putStrLn "No string"}}

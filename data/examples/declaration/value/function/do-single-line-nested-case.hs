nestedDo :: Either String Int -> IO ()
nestedDo e = do {putStr "Start: "; case e of {Left s -> do {putStr "Error: "; putStrLn s}; Right n -> do {putStr "Value: "; print n}}; putStrLn "End"}

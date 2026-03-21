getValue :: Maybe Int -> Int
getValue x = case x of {Just n -> n; Nothing -> 0}

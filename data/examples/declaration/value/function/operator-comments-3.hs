data X = X { x :: Int }

f = id
    . -- Some comment
    (\s -> s { x = 1 })

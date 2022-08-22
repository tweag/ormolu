foo :: Int -> Int
foo = id
{-# NOINLINE foo #-}

{-# NOINLINE [2] bar    #-}

bar :: Int -> Int
bar = id

baz :: Int -> Int
baz = id

{-#   NOINLINE    [~2] baz #-}

blub :: Int -> Int
blub = baz
{-# opaque blub #-}

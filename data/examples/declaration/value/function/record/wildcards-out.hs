{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

foo x y = Foo {x, y}

bar x y z =
  Bar
    { x
    , y
    , z
    , ..
    }

baz = Baz {..}

foo :: Boom -> Int
foo Boom {..} = 10

bar0 :: Boom -> Int
bar0 Boom {boom} = boom

bar1 :: Boom -> Int
bar1 Boom {boom = b} = b

baz :: Boom -> Int
baz Boom {boom = b, ..} = b

quux :: Boom -> Int
quux
  Boom
    { boom = a
    , foom = b
    , ..
    } = a + b

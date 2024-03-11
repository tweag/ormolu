vshow :: forall a -> Show a => a -> String
vshow t x = show (x :: t)

s1 = vshow Int    42
s2 = vshow Double 42

a1 = f (type (Int -> Bool))
a2 = f (type (Read T => T))
a3 = f (type (forall a. a))
a4 = f (type (forall a. Read a => String -> a))

foo = f (type (Maybe
            Int))

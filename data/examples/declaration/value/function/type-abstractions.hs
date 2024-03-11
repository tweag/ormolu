id :: forall a. a -> a
id @t x = x :: t

f1 :: forall a. a -> forall b. b -> (a, b)
f1 @a x @b y = (x :: a, y :: b)

f2 = (\ @a x @b y -> (x :: a, y :: b) )
    :: forall a. a -> forall b. b -> (a, b)

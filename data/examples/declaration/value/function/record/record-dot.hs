{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

data Foo = Foo { bar :: Foo }

mfoo = fmap (.bar)   $ Nothing

bar = (  Foo 1).bar

fooplus f n = f{foo = f.bar + n}

nestedFoo f = f.bar.bar.bar.bar.bar

nestedFooUpdate f = f {bar.bar = f.bar } <> f {bar.bar.bar.bar}

operatorUpdate f = f { (+) = 1 }

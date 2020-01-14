{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

data Foo = Foo {bar :: Int}

mfoo = fmap (.bar) $ Nothing

bar = (Foo 1).bar

fooplus f n = f{foo = f.bar + n}

fooplus' f n = f {foo = f.bar + n}

fooplus'' f n = f {foo = f.bar + n}

fooplus''' f n = f {foo = f.bar + n}

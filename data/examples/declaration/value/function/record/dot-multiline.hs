{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
bar' = (Foo 1){bar = 2
              }

fooplus'''' f n = f{foo = n,
                    bar = n
                   }

fooplus''''' f n = f
                   { foo = n }

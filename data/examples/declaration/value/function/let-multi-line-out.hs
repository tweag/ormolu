foo :: Int -> Int
foo x =
  let z = y
      y = x
   in z + 100

bar :: Int -> Int
bar x =
  let z = y
      y = x
   in z
        + 100

inlineComment :: Int -> Int
inlineComment =
  let {- join -} go = case () of
                   () -> undefined
   in go

implicitParams :: HasCallStack => Int
implicitParams =
  let ?cs = ?callstack
   in foo cs

sitting =
  foo $
    let x = 20
     in x

{-# LANGUAGE TemplateHaskell #-}

x = $(foo bar)

x =
  $( foo
       bar
   )

x = $foo

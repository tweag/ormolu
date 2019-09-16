{-# LANGUAGE TemplateHaskell #-}

singleLine = case () of
  $x -> ()
  $(y "something") -> ()

multiline = case () of
  $( x
       + y
   ) -> ()
  $( y
       "something"
   ) -> ()

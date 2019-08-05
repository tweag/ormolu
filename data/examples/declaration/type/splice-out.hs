{-# LANGUAGE TemplateHaskell #-}

type Foo = $(bar [t|Int|])

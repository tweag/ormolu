module Main where

-- | Documentation.
type Foo a b c = Bar c a b

type a ~> b = TyFun a b -> Type

type (a :+: b) c d e = ()

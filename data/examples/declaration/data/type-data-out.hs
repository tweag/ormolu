type data Universe = Character | Number | Boolean

type data Maybe a
  = Just a
  | Nothing

type data P :: Type -> Type -> Type where
  MkP :: (a ~ Natural, b ~~ Char) => P a b

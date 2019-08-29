{-# LANGUAGE UnicodeSyntax #-}

foo ∷ ∀a. Show a ⇒ a → String
foo = const ()

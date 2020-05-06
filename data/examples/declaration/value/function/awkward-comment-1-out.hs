doForeign :: Vars -> [Name] -> [Term] -> Idris LExp
doForeign x = x
  where
    splitArg tm | (_, [_, _, l, r]) <- unApply tm -- pair, two implicits
      =
      do
        let l' = toFDesc l
        r' <- irTerm (sMN 0 "__foreignCall") vs env r
        return (l', r')

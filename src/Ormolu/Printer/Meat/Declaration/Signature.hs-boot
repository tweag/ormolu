module Ormolu.Printer.Meat.Declaration.Signature
  ( p_sigDecl,
    p_typeAscription,
    p_activation,
  )
where

import GHC.Hs
import GHC.Types.Basic
import Ormolu.Printer.Combinators

p_sigDecl :: Sig GhcPs -> R ()
p_typeAscription :: LHsSigType GhcPs -> R ()
p_activation :: Activation -> R ()

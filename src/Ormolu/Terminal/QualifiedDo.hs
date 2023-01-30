module Ormolu.Terminal.QualifiedDo ((>>)) where

import Ormolu.Terminal
import Prelude hiding ((>>))

(>>) :: Term -> Term -> Term
(>>) = (<>)

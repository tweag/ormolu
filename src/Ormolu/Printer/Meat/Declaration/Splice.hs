{-# LANGUAGE LambdaCase #-}

module Ormolu.Printer.Meat.Declaration.Splice
  ( p_spliceDecl,
  )
where

import GHC.Hs
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Value (p_hsUntypedSplice)

p_spliceDecl :: SpliceDecl GhcPs -> R ()
p_spliceDecl (SpliceDecl NoExtField splice deco) =
  located splice $ p_hsUntypedSplice deco

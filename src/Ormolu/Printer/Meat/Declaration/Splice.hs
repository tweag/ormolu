{-# LANGUAGE LambdaCase #-}

module Ormolu.Printer.Meat.Declaration.Splice
  ( p_spliceDecl,
  )
where

import GHC.Hs
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Value (p_hsSplice)

p_spliceDecl :: SpliceDecl GhcPs -> R ()
p_spliceDecl = \case
  SpliceDecl NoExtField splice _explicit -> located splice p_hsSplice

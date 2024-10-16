{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Default
  ( p_defaultDecl,
  )
where

import GHC.Data.Maybe (whenIsJust)
import GHC.Hs
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type

p_defaultDecl :: DefaultDecl GhcPs -> R ()
p_defaultDecl (DefaultDecl _ mclass ts) = do
  txt "default"
  whenIsJust mclass $ \c -> do
    breakpoint
    p_rdrName c
  breakpoint
  inci . parens N $
    sep commaDel (sitcc . located' p_hsType) ts

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of type synonym declarations.

module Ormolu.Printer.Meat.Declaration.Type
  ( p_synDecl
  )
where

import Control.Monad
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import RdrName (RdrName (..))
import SrcLoc (Located)

p_synDecl
  :: Located RdrName            -- ^ Type constructor
  -> LHsQTyVars GhcPs           -- ^ Type variables
  -> LHsType GhcPs              -- ^ RHS of type declaration
  -> R ()
p_synDecl name tvars t = line $ do
  txt "type "
  p_rdrName name
  let HsQTvs {..} = tvars
  unless (null hsq_explicit) space
  spaceSep (located' p_hsTyVarBndr) hsq_explicit
  breakpoint
  inci (txt "= " >> located t p_hsType)

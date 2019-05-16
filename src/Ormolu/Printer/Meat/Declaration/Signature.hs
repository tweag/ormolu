{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type signature declarations.

module Ormolu.Printer.Meat.Declaration.Signature
  ( p_sigDecl )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type

p_sigDecl :: Sig GhcPs -> R ()
p_sigDecl = \case
  TypeSig names hswc -> line (p_typeSig names hswc)
  _ -> error "Ormolu.Printer.Meat.Declaration.Signature: unimplemented signatures"

p_typeSig
  :: [Located RdrName]
  -> LHsSigWcType GhcPs
  -> R ()
p_typeSig names HsWC {..} = do
  spaceSep (located' p_rdrName) names
  breakpoint
  inci $ do
    txt ":: "
    relaxComments $ located (hsib_body hswc_body) p_hsType

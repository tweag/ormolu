{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type signature declarations.

module Ormolu.Printer.Meat.Declaration.Signature
  ( p_sigDecl
  , p_sigDecl'
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_sigDecl :: Sig GhcPs -> R ()
p_sigDecl = line . p_sigDecl'

p_sigDecl' :: Sig GhcPs -> R ()
p_sigDecl' = \case
  TypeSig NoExt names hswc -> p_typeSig names hswc
  _ -> notImplemented "certain types of signature declarations"

p_typeSig
  :: [Located RdrName]
  -> LHsSigWcType GhcPs
  -> R ()
p_typeSig names HsWC {..} = do
  spaceSep p_rdrName names
  breakpoint
  inci $ do
    txt ":: "
    located (hsib_body hswc_body) p_hsType
p_typeSig _ (XHsWildCardBndrs NoExt) = notImplemented "XHsWildCardBndrs"

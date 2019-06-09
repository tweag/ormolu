{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Type signature declarations.

module Ormolu.Printer.Meat.Declaration.Signature
  ( p_sigDecl
  , p_sigDecl'
  )
where

import BasicTypes
import Control.Monad
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
  ClassOpSig NoExt def names hsib -> p_classOpSig def names hsib
  FixSig NoExt sig -> p_fixSig sig
  InlineSig NoExt name inlinePragma -> p_inlineSig name inlinePragma
  _ -> notImplemented "certain types of signature declarations"

p_typeSig
  :: [Located RdrName]          -- ^ Names (before @::@)
  -> LHsSigWcType GhcPs         -- ^ Type
  -> R ()
p_typeSig names HsWC {..} = do
  spaceSep p_rdrName names
  breakpoint
  inci $ do
    txt ":: "
    located (hsib_body hswc_body) p_hsType
p_typeSig _ (XHsWildCardBndrs NoExt) = notImplemented "XHsWildCardBndrs"

p_classOpSig
  :: Bool                       -- ^ Whether this is a \"default\" signature
  -> [Located RdrName]          -- ^ Names (before @::@)
  -> HsImplicitBndrs GhcPs (LHsType GhcPs) -- ^ Type
  -> R ()
p_classOpSig def names hsib =  do
  when def (txt "default ")
  p_typeSig names HsWC {hswc_ext = NoExt, hswc_body = hsib}

p_fixSig
  :: FixitySig GhcPs
  -> R ()
p_fixSig = \case
  FixitySig NoExt names (Fixity _ n dir) -> do
    txt $ case dir of
      InfixL -> "infixl"
      InfixR -> "infixr"
      InfixN -> "infix"
    space
    atom n
    space
    sequence_ (withSep comma p_rdrName names)
  XFixitySig NoExt -> notImplemented "XFixitySig"

p_inlineSig
  :: Located RdrName            -- ^ Name
  -> InlinePragma               -- ^ Inline pragma specification
  -> R ()
p_inlineSig name InlinePragma {..} = pragmaBraces $ do
  txt $ case inl_inline of
    Inline -> "INLINE"
    Inlinable -> "INLINEABLE"
    NoInline -> "NOINLINE"
    NoUserInline -> notImplemented "NoUserInline"
  space
  case inl_act of
    NeverActive -> return ()
    AlwaysActive -> return ()
    ActiveBefore _ n -> do
      brackets (txt "~" >> atom n)
      space
    ActiveAfter _ n -> do
      brackets (atom n)
      space
  p_rdrName name

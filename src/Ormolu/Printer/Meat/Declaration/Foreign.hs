{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Foreign
  ( p_foreignDecl,
  )
where

import Control.Monad
import GHC.Hs
import GHC.Types.ForeignCall
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Signature

p_foreignDecl :: ForeignDecl GhcPs -> R ()
p_foreignDecl = \case
  fd@ForeignImport {fd_fi} -> do
    p_foreignImport fd_fi
    p_foreignTypeSig fd
  fd@ForeignExport {fd_fe} -> do
    p_foreignExport fd_fe
    p_foreignTypeSig fd

-- | Printer for the last part of an import\/export, which is function name
-- and type signature.
p_foreignTypeSig :: ForeignDecl GhcPs -> R ()
p_foreignTypeSig fd = do
  breakpoint
  inci
    . switchLayout
      [ getLocA (fd_name fd),
        (getLocA . fd_sig_ty) fd
      ]
    $ do
      p_rdrName (fd_name fd)
      p_typeAscription (fd_sig_ty fd)

-- | Printer for 'ForeignImport'.
--
-- These have the form:
--
-- > foreign import callingConvention [safety] [identifier]
--
-- We need to check whether the safety has a good source, span, as it
-- defaults to 'PlaySafe' if you don't have anything in the source.
--
-- We also layout the identifier using the 'SourceText', because printing
-- with the other two fields of 'CImport' is very complicated. See the
-- 'Outputable' instance of 'ForeignImport' for details.
p_foreignImport :: ForeignImport -> R ()
p_foreignImport (CImport cCallConv safety _ _ sourceText) = do
  txt "foreign import"
  space
  located cCallConv atom
  -- Need to check for 'noLoc' for the 'safe' annotation
  when (isGoodSrcSpan $ getLoc safety) (space >> atom safety)
  space
  located sourceText p_sourceText

p_foreignExport :: ForeignExport -> R ()
p_foreignExport (CExport (L loc (CExportStatic _ _ cCallConv)) sourceText) = do
  txt "foreign export"
  space
  located (L loc cCallConv) atom
  space
  located sourceText p_sourceText

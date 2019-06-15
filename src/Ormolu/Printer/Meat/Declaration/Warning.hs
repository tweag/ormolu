{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Warning
  ( p_warnDecls
  , p_moduleWarning
  )
where

import BasicTypes
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils
import SrcLoc (combineSrcSpans)

p_warnDecls :: WarnDecls GhcPs -> R ()
p_warnDecls (Warnings NoExt _ warnings) =
  traverse_ (located' p_warnDecl) warnings
p_warnDecls XWarnDecls {} = notImplemented "XWarnDecls"

p_warnDecl :: WarnDecl GhcPs -> R ()
p_warnDecl (Warning NoExt functions warningTxt) =
  p_topLevelWarning functions warningTxt
p_warnDecl XWarnDecl {} = notImplemented "XWarnDecl"

p_moduleWarning :: WarningTxt -> R ()
p_moduleWarning wtxt = do
  let (pragmaText, lits) = warningText wtxt
  switchLayout (listSpan lits) $ do
    breakpoint
    inci $ pragma pragmaText (p_lits lits)

p_topLevelWarning :: [Located RdrName] -> WarningTxt -> R ()
p_topLevelWarning fnames wtxt = do
  let (pragmaText, lits) = warningText wtxt
  switchLayout (combineSrcSpans (listSpan fnames) (listSpan lits)) $ do
    pragma pragmaText $ do
      velt (withSep comma p_rdrName fnames)
      breakpoint
      p_lits lits

warningText :: WarningTxt -> (Text, [Located StringLiteral])
warningText = \case
  WarningTxt _ lits -> ("WARNING", lits)
  DeprecatedTxt _ lits -> ("DEPRECATED", lits)

listSpan :: [Located a] -> SrcSpan
listSpan xs = combineSrcSpans' (getLoc <$> NE.fromList xs)

p_lits :: [Located StringLiteral] -> R ()
p_lits = \case
  [l] -> atom l
  ls -> brackets . velt $ withSep comma atom ls

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Warning
  ( p_warnDecls,
    p_moduleWarning,
  )
where

import Data.Foldable
import Data.Text (Text)
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Unit.Module.Warnings
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_warnDecls :: WarnDecls GhcPs -> R ()
p_warnDecls (Warnings _ warnings) =
  traverse_ (located' p_warnDecl) warnings

p_warnDecl :: WarnDecl GhcPs -> R ()
p_warnDecl (Warning _ functions warningTxt) =
  p_topLevelWarning functions warningTxt

p_moduleWarning :: WarningTxt GhcPs -> R ()
p_moduleWarning wtxt = do
  let (pragmaText, lits) = warningText wtxt
  inci $ pragma pragmaText $ inci $ p_lits lits

p_topLevelWarning :: [LocatedN RdrName] -> WarningTxt GhcPs -> R ()
p_topLevelWarning fnames wtxt = do
  let (pragmaText, lits) = warningText wtxt
  switchLayout (fmap getLocA fnames ++ fmap getLoc lits) $
    pragma pragmaText . inci $ do
      sep commaDel p_rdrName fnames
      breakpoint
      p_lits lits

warningText :: WarningTxt GhcPs -> (Text, [Located StringLiteral])
warningText = \case
  WarningTxt _ lits -> ("WARNING", fmap hsDocString <$> lits)
  DeprecatedTxt _ lits -> ("DEPRECATED", fmap hsDocString <$> lits)

p_lits :: [Located StringLiteral] -> R ()
p_lits = \case
  [l] -> atom l
  ls -> brackets N $ sep commaDel atom ls

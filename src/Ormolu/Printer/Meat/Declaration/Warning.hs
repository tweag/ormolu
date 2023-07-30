{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Printer.Meat.Declaration.Warning
  ( p_warnDecls,
    p_warningTxt,
  )
where

import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Unit.Module.Warnings
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils

p_warnDecls :: WarnDecls GhcPs -> R ()
p_warnDecls (Warnings _ warnings) =
  traverse_ (located' p_warnDecl) warnings

p_warnDecl :: WarnDecl GhcPs -> R ()
p_warnDecl (Warning _ functions warningTxt) =
  p_topLevelWarning functions warningTxt

p_warningTxt :: WarningTxt GhcPs -> R ()
p_warningTxt wtxt = do
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
  WarningTxt mcat _ lits -> ("WARNING" <> T.pack cat, fmap hsDocString <$> lits)
    where
      cat = case unLoc <$> mcat of
        Just InWarningCategory {..} ->
          " in " <> show (showOutputable @WarningCategory (unLoc iwc_wc))
        Nothing -> ""
  DeprecatedTxt _ lits -> ("DEPRECATED", fmap hsDocString <$> lits)

p_lits :: [Located StringLiteral] -> R ()
p_lits = \case
  [l] -> atom l
  ls -> brackets N $ sep commaDel atom ls

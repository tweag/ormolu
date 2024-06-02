{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of import and export lists.
module Ormolu.Printer.Meat.ImportExport
  ( p_hsmodExports,
    p_hsmodImport,
  )
where

import Control.Monad
import Data.Choice (pattern Without)
import Data.Foldable (for_, traverse_)
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Types.PkgQual
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Utils (RelativePos (..), attachRelativePos)

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports xs =
  parens N $ do
    layout <- getLayout
    sep
      breakpoint
      (\(p, l) -> sitcc (located (addDocSrcSpan l) (p_lie layout p)))
      (attachRelativePos xs)
  where
    -- In order to correctly set the layout when a doc comment is present.
    addDocSrcSpan lie@(L l ie) = case ieExportDoc ie of
      Nothing -> lie
      Just (L l' _) -> L (l <> noAnnSrcSpan l') ie

p_hsmodImport :: ImportDecl GhcPs -> R ()
p_hsmodImport ImportDecl {..} = do
  useQualifiedPost <- isExtensionEnabled ImportQualifiedPost
  txt "import"
  space
  when (ideclSource == IsBoot) (txt "{-# SOURCE #-}")
  space
  when ideclSafe (txt "safe")
  space
  when
    (isImportDeclQualified ideclQualified && not useQualifiedPost)
    (txt "qualified")
  space
  case ideclPkgQual of
    NoRawPkgQual -> return ()
    RawPkgQual slit -> atom slit
  space
  inci $ do
    located ideclName atom
    when
      (isImportDeclQualified ideclQualified && useQualifiedPost)
      (space >> txt "qualified")
    case ideclAs of
      Nothing -> return ()
      Just l -> do
        space
        txt "as"
        space
        located l atom
    space
    case ideclImportList of
      Nothing -> return ()
      Just (hiding, L _ xs) -> do
        case hiding of
          Exactly -> pure ()
          EverythingBut -> txt "hiding"
        breakpoint
        parens N $ do
          layout <- getLayout
          sep
            breakpoint
            (\(p, l) -> sitcc (located l (p_lie layout p)))
            (attachRelativePos xs)
    newline

p_lie :: Layout -> RelativePos -> IE GhcPs -> R ()
p_lie encLayout relativePos = \case
  IEVar mwarn l1 exportDoc -> do
    for_ mwarn $ \warnTxt -> do
      located warnTxt p_warningTxt
      breakpoint
    located l1 p_ieWrappedName
    p_comma
    p_exportDoc exportDoc
  IEThingAbs _ l1 exportDoc -> do
    located l1 p_ieWrappedName
    p_comma
    p_exportDoc exportDoc
  IEThingAll _ l1 exportDoc -> do
    located l1 p_ieWrappedName
    space
    txt "(..)"
    p_comma
    p_exportDoc exportDoc
  IEThingWith _ l1 w xs exportDoc -> do
    sitcc $ do
      located l1 p_ieWrappedName
      breakpoint
      inci $ do
        let names :: [R ()]
            names = located' p_ieWrappedName <$> xs
        parens N . sep commaDel sitcc $
          case w of
            NoIEWildcard -> names
            IEWildcard n ->
              let (before, after) = splitAt n names
               in before ++ [txt ".."] ++ after
      p_comma
    p_exportDoc exportDoc
  IEModuleContents _ l1 -> do
    located l1 p_hsmodName
    p_comma
  IEGroup NoExtField n str -> do
    case relativePos of
      SinglePos -> return ()
      FirstPos -> return ()
      MiddlePos -> newline
      LastPos -> newline
    p_hsDoc (Asterisk n) (Without #endNewline) str
  IEDoc NoExtField str ->
    p_hsDoc Pipe (Without #endNewline) str
  IEDocNamed NoExtField str -> p_hsDocName str
  where
    p_comma =
      case encLayout of
        SingleLine ->
          case relativePos of
            SinglePos -> return ()
            FirstPos -> comma
            MiddlePos -> comma
            LastPos -> return ()
        MultiLine -> comma

    -- This is used to support `@since` annotations for (re)exported items. It
    -- /must/ use caret style comments, see
    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12098 and
    -- https://github.com/haskell/haddock/issues/1629#issuecomment-1931354411.
    p_exportDoc :: Maybe (ExportDoc GhcPs) -> R ()
    p_exportDoc = traverse_ $ \exportDoc -> do
      breakpoint
      p_hsDoc Caret (Without #endNewline) exportDoc

ieExportDoc :: IE GhcPs -> Maybe (ExportDoc GhcPs)
ieExportDoc = \case
  IEVar _ _ doc -> doc
  IEThingAbs _ _ doc -> doc
  IEThingAll _ _ doc -> doc
  IEThingWith _ _ _ _ doc -> doc
  IEModuleContents {} -> Nothing
  IEGroup {} -> Nothing
  IEDoc {} -> Nothing
  IEDocNamed {} -> Nothing

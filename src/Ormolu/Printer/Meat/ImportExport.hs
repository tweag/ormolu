{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of import and export lists.
module Ormolu.Printer.Meat.ImportExport
  ( p_hsmodExports,
    p_hsmodImport,
  )
where

import Control.Monad
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Types.PkgQual
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils (RelativePos (..), attachRelativePos)

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports [] = do
  txt "("
  breakpoint'
  txt ")"
p_hsmodExports xs =
  parens N $ do
    layout <- getLayout
    sep
      breakpoint
      (\(p, l) -> sitcc (located l (p_lie layout p)))
      (attachRelativePos xs)

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
  IEVar NoExtField l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAbs _ l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAll _ l1 -> do
    located l1 p_ieWrappedName
    space
    txt "(..)"
    p_comma
  IEThingWith _ l1 w xs -> sitcc $ do
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
  IEModuleContents _ l1 -> do
    located l1 p_hsmodName
    p_comma
  IEGroup NoExtField n str -> do
    case relativePos of
      SinglePos -> return ()
      FirstPos -> return ()
      MiddlePos -> newline
      LastPos -> newline
    p_hsDoc (Asterisk n) False str
  IEDoc NoExtField str ->
    p_hsDoc Pipe False str
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

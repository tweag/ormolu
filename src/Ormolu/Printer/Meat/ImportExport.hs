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
import GHC
import HsImpExp (IE (..))
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports [] = do
  txt "("
  breakpoint'
  txt ")"
p_hsmodExports xs =
  parens N . sitcc $ do
    layout <- getLayout
    sep breakpoint (sitcc . located' (uncurry (p_lie layout))) (attachPositions xs)

p_hsmodImport :: ImportDecl GhcPs -> R ()
p_hsmodImport ImportDecl {..} = do
  txt "import"
  space
  when ideclSource (txt "{-# SOURCE #-}")
  space
  when ideclSafe (txt "safe")
  space
  when ideclQualified (txt "qualified")
  space
  case ideclPkgQual of
    Nothing -> return ()
    Just slit -> atom slit
  space
  inci $ do
    located ideclName atom
    case ideclAs of
      Nothing -> return ()
      Just l -> do
        space
        txt "as"
        space
        located l atom
    space
    case ideclHiding of
      Nothing -> return ()
      Just (hiding, _) ->
        when hiding (txt "hiding")
    case ideclHiding of
      Nothing -> return ()
      Just (_, (L _ xs)) -> do -- FIXME
        breakpoint
        parens N . sitcc $ do
          layout <- getLayout
          sep
            breakpoint
            (sitcc . located' (uncurry (p_lie layout)))
            (attachPositions xs)
    newline
p_hsmodImport (XImportDecl NoExt) = notImplemented "XImportDecl"

p_lie :: Layout -> (Int, Int) -> IE GhcPs -> R ()
p_lie encLayout (i, totalItems) = \case
  IEVar NoExt l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAbs NoExt l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAll NoExt l1 -> do
    located l1 p_ieWrappedName
    space
    txt "(..)"
    p_comma
  IEThingWith NoExt l1 w xs _ -> sitcc $ do
    located l1 p_ieWrappedName
    breakpoint
    inci $ do
      let names :: [R ()]
          names = located' p_ieWrappedName <$> xs
      parens N . sitcc
        $ sep (comma >> breakpoint) sitcc
        $ case w of
          NoIEWildcard -> names
          IEWildcard n ->
            let (before, after) = splitAt n names
             in before ++ [txt ".."] ++ after
    p_comma
  IEModuleContents NoExt l1 -> do
    located l1 p_hsmodName
    p_comma
  IEGroup NoExt n str -> do
    unless (i == 0) newline
    p_hsDocString (Asterisk n) False (noLoc str)
  IEDoc NoExt str ->
    p_hsDocString Pipe False (noLoc str)
  IEDocNamed NoExt str -> p_hsDocName str
  XIE NoExt -> notImplemented "XIE"
  where
    p_comma =
      case encLayout of
        SingleLine -> unless (i + 1 == totalItems) comma
        MultiLine -> comma

-- | Attach positions to 'Located' things in a list.
attachPositions ::
  [Located a] ->
  [Located ((Int, Int), a)]
attachPositions xs =
  let f i (L l x) = L l ((i, n), x)
      n = length xs
   in zipWith f [0 ..] xs

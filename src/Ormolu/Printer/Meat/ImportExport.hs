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
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports [] = do
  txt "("
  breakpoint'
  txt ")"
p_hsmodExports xs =
  parens N . sitcc $ do
    layout <- getLayout
    sep breakpoint (sitcc . located' (uncurry (p_lie layout))) (attachPositions xs)

p_hsmodImport :: Bool -> ImportDecl GhcPs -> R ()
p_hsmodImport useQualifiedPost ImportDecl {..} = do
  txt "import"
  space
  when ideclSource (txt "{-# SOURCE #-}")
  space
  when ideclSafe (txt "safe")
  space
  when
    (isImportDeclQualified ideclQualified && not useQualifiedPost)
    (txt "qualified")
  space
  case ideclPkgQual of
    Nothing -> return ()
    Just slit -> atom slit
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
    case ideclHiding of
      Nothing -> return ()
      Just (hiding, _) ->
        when hiding (txt "hiding")
    case ideclHiding of
      Nothing -> return ()
      Just (_, L _ xs) -> do
        breakpoint
        parens N . sitcc $ do
          layout <- getLayout
          sep breakpoint (sitcc . located' (uncurry (p_lie layout))) (attachPositions xs)
    newline
p_hsmodImport _ (XImportDecl x) = noExtCon x

p_lie :: Layout -> (Int, Int) -> IE GhcPs -> R ()
p_lie encLayout (i, totalItems) = \case
  IEVar NoExtField l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAbs NoExtField l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAll NoExtField l1 -> do
    located l1 p_ieWrappedName
    space
    txt "(..)"
    p_comma
  IEThingWith NoExtField l1 w xs _ -> sitcc $ do
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
  IEModuleContents NoExtField l1 -> do
    located l1 p_hsmodName
    p_comma
  IEGroup NoExtField n str -> do
    unless (i == 0) newline
    p_hsDocString (Asterisk n) False (noLoc str)
  IEDoc NoExtField str ->
    p_hsDocString Pipe False (noLoc str)
  IEDocNamed NoExtField str -> p_hsDocName str
  XIE x -> noExtCon x
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

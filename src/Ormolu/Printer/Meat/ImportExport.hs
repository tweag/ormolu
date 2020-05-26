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
  parens N (p_lies xs)

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
        parens N (p_lies xs)
    newline
p_hsmodImport _ (XImportDecl x) = noExtCon x

p_lies :: [LIE GhcPs] -> R ()
p_lies = go True True
  where
    go _ _ [] = return ()
    go isFirstElement isFirstItem (x:xs)= do
      let thisIsItem = isIEItem (unLoc x)
      when (thisIsItem && not isFirstItem) $
        comma >> space
      inci $ located x (p_lie isFirstElement)
      unless (null xs) breakpoint'
      go False (if thisIsItem then False else isFirstItem) xs

p_lie :: Bool -> IE GhcPs -> R ()
p_lie isFirstElement = \case
  IEVar NoExtField l1 ->
    located l1 p_ieWrappedName
  IEThingAbs NoExtField l1 ->
    located l1 p_ieWrappedName
  IEThingAll NoExtField l1 -> do
    located l1 p_ieWrappedName
    space
    txt "(..)"
  IEThingWith NoExtField l1 w xs _ -> sitcc $ do
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
  IEModuleContents NoExtField l1 ->
    located l1 p_hsmodName
  IEGroup NoExtField n str -> do
    unless isFirstElement (newline >> newline)
    p_hsDocString (Asterisk n) False (noLoc str)
  IEDoc NoExtField str ->
    p_hsDocString Pipe False (noLoc str)
  IEDocNamed NoExtField str -> p_hsDocName str
  XIE x -> noExtCon x

isIEItem :: IE GhcPs -> Bool
isIEItem = \case
  IEVar {} -> True
  IEThingAbs {} -> True
  IEThingAll {} -> True
  IEThingWith {} -> True
  IEModuleContents {} -> True
  _ -> False

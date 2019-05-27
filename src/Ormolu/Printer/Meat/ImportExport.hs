{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of import and export lists.

module Ormolu.Printer.Meat.ImportExport
  ( p_hsmodExports
  , p_hsmodImport
  )
where

import Control.Monad
import GHC
import HsImpExp (IE (..))
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports xs = do
  parens . velt $ withSep comma (sitcc . located' p_lie) xs

p_hsmodImport :: ImportDecl GhcPs -> R ()
p_hsmodImport ImportDecl {..} = line $ do
  txt "import "
  when ideclSource $
    txt "{-# SOURCE #-} "
  when ideclSafe $
    txt "safe "
  when ideclQualified $
    txt "qualified "
  case ideclPkgQual of
    Nothing -> return ()
    Just slit -> do
      atom slit
      space
  located ideclName atom
  case ideclAs of
    Nothing -> return ()
    Just l -> do
      txt " as "
      located l atom
  case ideclHiding of
    Nothing -> return ()
    Just (hiding, _) ->
      when hiding $
        txt " hiding"
  case ideclHiding of
    Nothing -> return ()
    Just (_, l) -> do
      breakpoint
      inci . locatedVia Nothing l $
        parens . velt . withSep comma (located' p_lie)
p_hsmodImport (XImportDecl NoExt) = notImplemented "XImportDecl"

p_lie :: IE GhcPs -> R ()
p_lie = \case
  IEVar NoExt l1 -> located l1 p_ieWrappedName
  IEThingAbs NoExt l1 -> located l1 p_ieWrappedName
  IEThingAll NoExt l1 -> do
    located l1 p_ieWrappedName
    txt " (..)"
  IEThingWith NoExt l1 w xs _ -> sitcc $ do
    located l1 p_ieWrappedName
    breakpoint
    -- XXX I have no idea what field labels are in this context.
    inci $ do
      p_ieWildcard w
      parens . velt $ withSep comma (located' p_ieWrappedName) xs
  IEModuleContents NoExt l1 -> located l1 p_hsmodName
  -- XXX I have no idea what these things are for.
  IEGroup NoExt _ _ -> return ()
  IEDoc NoExt _ -> return ()
  IEDocNamed NoExt _ -> return ()
  XIE NoExt -> notImplemented "XIE"

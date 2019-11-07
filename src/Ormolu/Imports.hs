{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Manipulations on import lists.
module Ormolu.Imports
  ( sortImports,
  )
where

import Data.Bifunctor
import Data.Function (on)
import Data.Generics (gcompare)
import Data.List (sortBy)
import GHC hiding (GhcPs, IE)
import HsExtension
import HsImpExp (IE (..))
import Ormolu.Utils (notImplemented)

-- | Sort imports by module name. This also sorts explicit import lists for
-- each declaration.
sortImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImports = sortBy compareIdecl . fmap (fmap sortImportLists)
  where
    sortImportLists :: ImportDecl GhcPs -> ImportDecl GhcPs
    sortImportLists = \case
      ImportDecl {..} ->
        ImportDecl
          { ideclHiding = second (fmap sortLies) <$> ideclHiding,
            ..
          }
      XImportDecl {} -> notImplemented "XImportDecl"

-- | Compare two @'LImportDecl' 'GhcPs'@ things.
compareIdecl :: LImportDecl GhcPs -> LImportDecl GhcPs -> Ordering
compareIdecl (L _ m0) (L _ m1) =
  case (isPrelude n0, isPrelude n1) of
    (False, False) -> n0 `compare` n1
    (True, False) -> GT
    (False, True) -> LT
    (True, True) -> m0 `gcompare` m1
  where
    n0 = unLoc (ideclName m0)
    n1 = unLoc (ideclName m1)
    isPrelude = (== "Prelude") . moduleNameString

-- | Sort located import or export.
sortLies :: [LIE GhcPs] -> [LIE GhcPs]
sortLies = sortBy (compareIE `on` unLoc) . fmap (fmap sortThings)

-- | Sort imports\/exports inside of 'IEThingWith'.
sortThings :: IE GhcPs -> IE GhcPs
sortThings = \case
  IEThingWith NoExt x w xs fl ->
    IEThingWith NoExt x w (sortBy (compareIewn `on` unLoc) xs) fl
  other -> other

-- | Compare two located imports or exports.
compareIE :: IE GhcPs -> IE GhcPs -> Ordering
compareIE = compareIewn `on` getIewn

-- | Project @'IEWrappedName' 'RdrName'@ from @'IE' 'GhcPs'@.
getIewn :: IE GhcPs -> IEWrappedName RdrName
getIewn = \case
  IEVar NoExt x -> unLoc x
  IEThingAbs NoExt x -> unLoc x
  IEThingAll NoExt x -> unLoc x
  IEThingWith NoExt x _ _ _ -> unLoc x
  IEModuleContents NoExt _ -> notImplemented "IEModuleContents"
  IEGroup NoExt _ _ -> notImplemented "IEGroup"
  IEDoc NoExt _ -> notImplemented "IEDoc"
  IEDocNamed NoExt _ -> notImplemented "IEDocNamed"
  XIE NoExt -> notImplemented "XIE"

-- | Compare two @'IEWrapppedName' 'RdrName'@ things.
compareIewn :: IEWrappedName RdrName -> IEWrappedName RdrName -> Ordering
compareIewn (IEName x) (IEName y) = unLoc x `compare` unLoc y
compareIewn (IEName _) (IEPattern _) = LT
compareIewn (IEName _) (IEType _) = LT
compareIewn (IEPattern _) (IEName _) = GT
compareIewn (IEPattern x) (IEPattern y) = unLoc x `compare` unLoc y
compareIewn (IEPattern _) (IEType _) = LT
compareIewn (IEType _) (IEName _) = GT
compareIewn (IEType _) (IEPattern _) = GT
compareIewn (IEType x) (IEType y) = unLoc x `compare` unLoc y

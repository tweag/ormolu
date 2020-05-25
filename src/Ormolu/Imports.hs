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
import Data.List (nubBy, sortBy)
import GHC hiding (GhcPs, IE)
import GHC.Hs.Extension
import GHC.Hs.ImpExp (IE (..))
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
      XImportDecl x -> noExtCon x

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

-- | Sort located import or export, dropping duplicates.
sortLies :: [LIE GhcPs] -> [LIE GhcPs]
sortLies =
  nubBy (\x y -> compareLIE x y == EQ)
    . sortBy compareLIE
    . fmap (fmap sortThings)

-- | Sort imports\/exports inside of 'IEThingWith'.
sortThings :: IE GhcPs -> IE GhcPs
sortThings = \case
  IEThingWith NoExtField x w xs fl ->
    IEThingWith NoExtField x w (sortBy (compareIewn `on` unLoc) xs) fl
  other -> other

-- | Compare a pair of located imports or exports.
compareLIE :: LIE GhcPs -> LIE GhcPs -> Ordering
compareLIE = compareIewn `on` getIewn . unLoc

-- | Project @'IEWrappedName' 'RdrName'@ from @'IE' 'GhcPs'@.
getIewn :: IE GhcPs -> IEWrappedName RdrName
getIewn = \case
  IEVar NoExtField x -> unLoc x
  IEThingAbs NoExtField x -> unLoc x
  IEThingAll NoExtField x -> unLoc x
  IEThingWith NoExtField x _ _ _ -> unLoc x
  IEModuleContents NoExtField _ -> notImplemented "IEModuleContents"
  IEGroup NoExtField _ _ -> notImplemented "IEGroup"
  IEDoc NoExtField _ -> notImplemented "IEDoc"
  IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
  XIE x -> noExtCon x

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

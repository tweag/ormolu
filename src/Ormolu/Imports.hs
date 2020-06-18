{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Manipulations on import lists.
module Ormolu.Imports
  ( sortImports,
  )
where

import Data.Bifunctor
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Generics (gcompare)
import Data.List (foldl', nubBy, sortBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC hiding (GhcPs, IE)
import GHC.Hs.Extension
import GHC.Hs.ImpExp (IE (..))
import Ormolu.Utils (notImplemented, showOutputable)

-- | Sort imports by module name. This also sorts and normalizes explicit
-- import lists for each declaration.
sortImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImports = sortBy compareIdecl . fmap (fmap sortImportLists)
  where
    sortImportLists :: ImportDecl GhcPs -> ImportDecl GhcPs
    sortImportLists = \case
      ImportDecl {..} ->
        ImportDecl
          { ideclHiding = second (fmap normalizeLies) <$> ideclHiding,
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

-- | Normalize a collection of import\/export items.
normalizeLies :: [LIE GhcPs] -> [LIE GhcPs]
normalizeLies = sortOn (getIewn . unLoc) . M.elems . foldl' combine M.empty
  where
    combine ::
      Map IEWrappedNameOrd (LIE GhcPs) ->
      LIE GhcPs ->
      Map IEWrappedNameOrd (LIE GhcPs)
    combine m (L new_l new) =
      let wname = getIewn new
          normalizeWNames =
            nubBy (\x y -> compareLIewn x y == EQ) . sortBy compareLIewn
          alter = \case
            Nothing -> Just . L new_l $
              case new of
                IEThingWith NoExtField n wildcard g flbl ->
                  IEThingWith NoExtField n wildcard (normalizeWNames g) flbl
                other -> other
            Just old ->
              let f = \case
                    IEVar NoExtField n -> IEVar NoExtField n
                    IEThingAbs NoExtField _ -> new
                    IEThingAll NoExtField n -> IEThingAll NoExtField n
                    IEThingWith NoExtField n wildcard g flbl ->
                      case new of
                        IEVar NoExtField _ ->
                          error "Ormolu.Imports broken presupposition"
                        IEThingAbs NoExtField _ ->
                          IEThingWith NoExtField n wildcard g flbl
                        IEThingAll NoExtField n' ->
                          IEThingAll NoExtField n'
                        IEThingWith NoExtField n' wildcard' g' flbl' ->
                          let combinedWildcard =
                                case (wildcard, wildcard') of
                                  (IEWildcard _, _) -> IEWildcard 0
                                  (_, IEWildcard _) -> IEWildcard 0
                                  _ -> NoIEWildcard
                           in IEThingWith
                                NoExtField
                                n'
                                combinedWildcard
                                (normalizeWNames (g <> g'))
                                flbl'
                        IEModuleContents NoExtField _ -> notImplemented "IEModuleContents"
                        IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                        IEDoc NoExtField _ -> notImplemented "IEDoc"
                        IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
                        XIE x -> noExtCon x
                    IEModuleContents NoExtField _ -> notImplemented "IEModuleContents"
                    IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                    IEDoc NoExtField _ -> notImplemented "IEDoc"
                    IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
                    XIE x -> noExtCon x
               in Just (f <$> old)
       in M.alter alter wname m

-- | A wrapper for @'IEWrappedName' 'RdrName'@ that allows us to define an
-- 'Ord' instance for it.
newtype IEWrappedNameOrd = IEWrappedNameOrd (IEWrappedName RdrName)
  deriving (Eq)

instance Ord IEWrappedNameOrd where
  compare (IEWrappedNameOrd x) (IEWrappedNameOrd y) = compareIewn x y

-- | Project @'IEWrappedName' 'RdrName'@ from @'IE' 'GhcPs'@.
getIewn :: IE GhcPs -> IEWrappedNameOrd
getIewn = \case
  IEVar NoExtField x -> IEWrappedNameOrd (unLoc x)
  IEThingAbs NoExtField x -> IEWrappedNameOrd (unLoc x)
  IEThingAll NoExtField x -> IEWrappedNameOrd (unLoc x)
  IEThingWith NoExtField x _ _ _ -> IEWrappedNameOrd (unLoc x)
  IEModuleContents NoExtField _ -> notImplemented "IEModuleContents"
  IEGroup NoExtField _ _ -> notImplemented "IEGroup"
  IEDoc NoExtField _ -> notImplemented "IEDoc"
  IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
  XIE x -> noExtCon x

-- | Like 'compareIewn' for located wrapped names.
compareLIewn :: LIEWrappedName RdrName -> LIEWrappedName RdrName -> Ordering
compareLIewn = compareIewn `on` unLoc

-- | Compare two @'IEWrapppedName' 'RdrName'@ things.
compareIewn :: IEWrappedName RdrName -> IEWrappedName RdrName -> Ordering
compareIewn (IEName x) (IEName y) = unLoc x `compareRdrName` unLoc y
compareIewn (IEName _) (IEPattern _) = LT
compareIewn (IEName _) (IEType _) = LT
compareIewn (IEPattern _) (IEName _) = GT
compareIewn (IEPattern x) (IEPattern y) = unLoc x `compareRdrName` unLoc y
compareIewn (IEPattern _) (IEType _) = LT
compareIewn (IEType _) (IEName _) = GT
compareIewn (IEType _) (IEPattern _) = GT
compareIewn (IEType x) (IEType y) = unLoc x `compareRdrName` unLoc y

compareRdrName :: RdrName -> RdrName -> Ordering
compareRdrName x y =
  case (getNameStr x, getNameStr y) of
    ([], []) -> EQ
    ((_ : _), []) -> GT
    ([], (_ : _)) -> LT
    ((x' : _), (y' : _)) ->
      case (isAlphaNum x', isAlphaNum y') of
        (False, False) -> x `compare` y
        (True, False) -> LT
        (False, True) -> GT
        (True, True) -> x `compare` y
  where
    getNameStr = showOutputable . rdrNameOcc

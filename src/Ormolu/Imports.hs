{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Manipulations on import lists.

module Ormolu.Imports
  ( sortImports
  )
where

import Data.Bifunctor
import Data.Function (on)
import Data.List (sortBy)
import GHC hiding (GhcPs, IE)
import HsImpExp (IE (..))
import Language.Haskell.GHC.ExactPrint.Types

-- | Sort imports by module name. This also sorts explicit import lists for
-- each declaration.

sortImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImports = sortBy compareIdecl . fmap (fmap sortImportLists)
  where
    sortImportLists :: ImportDecl GhcPs -> ImportDecl GhcPs
    sortImportLists decl =
      decl { ideclHiding = second (fmap sortLies) <$> ideclHiding decl
           }

-- | Compare two @'LImportDecl' 'GhcPs'@ things.

compareIdecl :: LImportDecl GhcPs -> LImportDecl GhcPs -> Ordering
compareIdecl (L _ m0) (L _ m1) =
  if isPrelude n0
    then GT
    else if isPrelude n1
           then LT
           else n0 `compare` n1
  where
    n0 = unL (ideclName m0)
    n1 = unL (ideclName m1)
    isPrelude = (== "Prelude") . moduleNameString

-- | Sort located import or export.

sortLies :: [LIE GhcPs] -> [LIE GhcPs]
sortLies = sortBy (compareIE `on` unL) . fmap (fmap sortThings)

-- | Sort imports\/exports inside of 'IEThingWith'.

sortThings :: IE GhcPs -> IE GhcPs
sortThings = \case
  IEThingWith x w xs fl ->
    IEThingWith x w (sortBy (compareIewn `on` unL) xs) fl
  other -> other

-- | Compare two located imports or exports.

compareIE :: IE GhcPs -> IE GhcPs -> Ordering
compareIE = compareIewn `on` getIewn

-- | Project @'IEWrappedName' 'RdrName'@ from @'IE' 'GhcPs'@.

getIewn :: IE GhcPs -> IEWrappedName RdrName
getIewn = \case
  IEVar x -> unL x
  IEThingAbs x -> unL x
  IEThingAll x -> unL x
  IEThingWith x _ _ _ -> unL x
  IEModuleContents _ -> error "Ormolu.Imports.projectName: IEModuleContents"
  IEGroup _ _ -> error "Ormolu.Imports.projectName: IEGroup"
  IEDoc _ -> error "Ormolu.Imports.projectName: IEGroup"
  IEDocNamed _ -> error "Ormolu.Imports.projectName: IEGroup"

-- | Compare two @'IEWrapppedName' 'RdrName'@ things.

compareIewn :: IEWrappedName RdrName -> IEWrappedName RdrName -> Ordering
compareIewn (IEName x) (IEName y) = unL x `compare` unL y
compareIewn (IEName _) (IEPattern _) = LT
compareIewn (IEName _) (IEType _) = LT
compareIewn (IEPattern _) (IEName _) = GT
compareIewn (IEPattern x) (IEPattern y) = unL x `compare` unL y
compareIewn (IEPattern _) (IEType _) = LT
compareIewn (IEType _) (IEName _) = GT
compareIewn (IEType _) (IEPattern _) = GT
compareIewn (IEType x) (IEType y) = unL x `compare` unL y

-- | Exact inner value from 'Located'.

unL :: Located e -> e
unL (L _ e) = e

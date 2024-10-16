{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Manipulations on import lists.
module Ormolu.Imports
  ( normalizeImports,
  )
where

import Data.Bifunctor
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (nubBy, sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import GHC.Data.FastString
import GHC.Hs
import GHC.Hs.ImpExp as GHC
import GHC.Types.Name.Reader
import GHC.Types.PkgQual
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Ormolu.Utils (notImplemented, showOutputable)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif

-- | Sort and normalize imports.
normalizeImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
normalizeImports =
  fmap snd
    . M.toAscList
    . M.fromListWith combineImports
    . fmap (\x -> (importId x, g x))
  where
    g :: LImportDecl GhcPs -> LImportDecl GhcPs
    g (L l ImportDecl {..}) =
      L
        l
        ImportDecl
          { ideclImportList = second (fmap normalizeLies) <$> ideclImportList,
            ..
          }

-- | Combine two import declarations. It should be assumed that 'ImportId's
-- are equal.
combineImports ::
  LImportDecl GhcPs ->
  LImportDecl GhcPs ->
  LImportDecl GhcPs
combineImports (L lx ImportDecl {..}) (L _ y) =
  L
    lx
    ImportDecl
      { ideclImportList = case (ideclImportList, GHC.ideclImportList y) of
          (Just (hiding, L l' xs), Just (_, L _ ys)) ->
            Just (hiding, (L l' (normalizeLies (xs ++ ys))))
          _ -> Nothing,
        ..
      }

-- | Import id, a collection of all things that justify having a separate
-- import entry. This is used for merging of imports. If two imports have
-- the same 'ImportId' they can be merged.
data ImportId = ImportId
  { importIsPrelude :: Bool,
    importPkgQual :: ImportPkgQual,
    importIdName :: ModuleName,
    importSource :: IsBootInterface,
    importSafe :: Bool,
    importQualified :: Bool,
    importAs :: Maybe ModuleName,
    importHiding :: Maybe ImportListInterpretationOrd
  }
  deriving (Eq, Ord)

data ImportPkgQual
  = -- | The import is not qualified by a package name.
    NoImportPkgQual
  | -- | The import is qualified by an external package name.
    ImportPkgQual LexicalFastString
  | -- | The import is qualified by the current package being built, using the
    -- special @this@ package name.
    ImportPkgQualThis
  deriving stock (Eq, Ord)

mkImportPkgQual :: RawPkgQual -> ImportPkgQual
mkImportPkgQual = \case
  NoRawPkgQual -> NoImportPkgQual
  RawPkgQual (sl_fs -> fs)
    | fs == mkFastString "this" -> ImportPkgQualThis
    | otherwise -> ImportPkgQual (LexicalFastString fs)

-- | 'ImportListInterpretation' does not have an 'Ord' instance.
newtype ImportListInterpretationOrd = ImportListInterpretationOrd
  { unImportListInterpretationOrd :: ImportListInterpretation
  }
  deriving stock (Eq)

instance Ord ImportListInterpretationOrd where
  compare = compare `on` toBool . unImportListInterpretationOrd
    where
      toBool Exactly = False
      toBool EverythingBut = True

-- | Obtain an 'ImportId' for a given import.
importId :: LImportDecl GhcPs -> ImportId
importId (L _ ImportDecl {..}) =
  ImportId
    { importIsPrelude = isPrelude,
      importIdName = moduleName,
      importPkgQual = mkImportPkgQual ideclPkgQual,
      importSource = ideclSource,
      importSafe = ideclSafe,
      importQualified = case ideclQualified of
        QualifiedPre -> True
        QualifiedPost -> True
        NotQualified -> False,
      importAs = unLoc <$> ideclAs,
      importHiding = ImportListInterpretationOrd . fst <$> ideclImportList
    }
  where
    isPrelude = moduleNameString moduleName == "Prelude"
    moduleName = unLoc ideclName

-- | Normalize a collection of import items.
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
                IEThingWith x n wildcard g _ ->
                  IEThingWith x n wildcard (normalizeWNames g) Nothing
                other -> other
            Just old ->
              let f = \case
                    IEVar _ n _ -> IEVar Nothing n Nothing
                    IEThingAbs _ _ _ -> new
                    IEThingAll x n _ -> IEThingAll x n Nothing
                    IEThingWith _ n wildcard g _ ->
                      case new of
                        IEVar _ _ _ ->
                          error "Ormolu.Imports broken presupposition"
                        IEThingAbs x _ _ ->
                          IEThingWith (x, noAnn) n wildcard g Nothing
                        IEThingAll x n' _ ->
                          IEThingAll x n' Nothing
                        IEThingWith x n' wildcard' g' _ ->
                          let combinedWildcard =
                                case (wildcard, wildcard') of
                                  (IEWildcard _, _) -> IEWildcard 0
                                  (_, IEWildcard _) -> IEWildcard 0
                                  _ -> NoIEWildcard
                           in IEThingWith
                                x
                                n'
                                combinedWildcard
                                (normalizeWNames (g <> g'))
                                Nothing
                        IEModuleContents _ _ -> notImplemented "IEModuleContents"
                        IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                        IEDoc NoExtField _ -> notImplemented "IEDoc"
                        IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
                    IEModuleContents _ _ -> notImplemented "IEModuleContents"
                    IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                    IEDoc NoExtField _ -> notImplemented "IEDoc"
                    IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
               in Just (f <$> old)
       in M.alter alter wname m

-- | A wrapper for @'IEWrappedName' 'GhcPs'@ that allows us to define an
-- 'Ord' instance for it.
newtype IEWrappedNameOrd = IEWrappedNameOrd (IEWrappedName GhcPs)
  deriving (Eq)

instance Ord IEWrappedNameOrd where
  compare (IEWrappedNameOrd x) (IEWrappedNameOrd y) = compareIewn x y

-- | Project @'IEWrappedName' 'GhcPs'@ from @'IE' 'GhcPs'@.
getIewn :: IE GhcPs -> IEWrappedNameOrd
getIewn = \case
  IEVar _ x _ -> IEWrappedNameOrd (unLoc x)
  IEThingAbs _ x _ -> IEWrappedNameOrd (unLoc x)
  IEThingAll _ x _ -> IEWrappedNameOrd (unLoc x)
  IEThingWith _ x _ _ _ -> IEWrappedNameOrd (unLoc x)
  IEModuleContents _ _ -> notImplemented "IEModuleContents"
  IEGroup NoExtField _ _ -> notImplemented "IEGroup"
  IEDoc NoExtField _ -> notImplemented "IEDoc"
  IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"

-- | Like 'compareIewn' for located wrapped names.
compareLIewn :: LIEWrappedName GhcPs -> LIEWrappedName GhcPs -> Ordering
compareLIewn = compareIewn `on` unLoc

-- | Compare two @'IEWrapppedName' 'GhcPs'@ things.
compareIewn :: IEWrappedName GhcPs -> IEWrappedName GhcPs -> Ordering
compareIewn = (comparing fst <> (compareRdrName `on` unLoc . snd)) `on` classify
  where
    classify :: IEWrappedName GhcPs -> (Int, LocatedN RdrName)
    classify = \case
      IEName _ x -> (0, x)
      IEDefault _ x -> (1, x)
      IEPattern _ x -> (2, x)
      IEType _ x -> (3, x)

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

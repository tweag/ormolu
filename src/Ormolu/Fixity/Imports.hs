{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Simplified representation of the import list for the purposes of fixity
-- inference.
module Ormolu.Fixity.Imports
  ( FixityImport (..),
    extractFixityImports,
    applyModuleReexports,
  )
where

import Data.Bifunctor (second)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName
import GHC.Data.FastString qualified as GHC
import GHC.Hs hiding (ModuleName, OpName)
import GHC.Types.Name.Occurrence
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SourceText (StringLiteral (..))
import GHC.Types.SrcLoc
import Ormolu.Fixity.Internal
import Ormolu.Utils (ghcModuleNameToCabal)

-- | Simplified info about an import.
data FixityImport = FixityImport
  { fimportPackage :: !(Maybe PackageName),
    fimportModule :: !ModuleName,
    fimportQualified :: !FixityQualification,
    fimportList :: !(Maybe (ImportListInterpretation, [OpName]))
  }

-- | Extract 'FixityImport's from the AST.
extractFixityImports ::
  [LImportDecl GhcPs] ->
  [FixityImport]
extractFixityImports = fmap (extractFixityImport . unLoc)

-- | Extract an individual 'FixityImport'.
extractFixityImport :: ImportDecl GhcPs -> FixityImport
extractFixityImport ImportDecl {..} =
  FixityImport
    { fimportPackage = case ideclPkgQual of
        NoRawPkgQual -> Nothing
        RawPkgQual strLiteral ->
          Just . mkPackageName . GHC.unpackFS . sl_fs $ strLiteral,
      fimportModule = ideclName',
      fimportQualified = case (ideclQualified, ideclAs') of
        (QualifiedPre, Nothing) ->
          OnlyQualified ideclName'
        (QualifiedPost, Nothing) ->
          OnlyQualified ideclName'
        (QualifiedPre, Just m) -> OnlyQualified m
        (QualifiedPost, Just m) -> OnlyQualified m
        (NotQualified, Nothing) ->
          UnqualifiedAndQualified ideclName'
        (NotQualified, Just m) ->
          UnqualifiedAndQualified m,
      fimportList =
        fmap
          (second (concatMap (fmap occOpName . ieToOccNames . unLoc) . unLoc))
          ideclImportList
    }
  where
    ideclName' = ghcModuleNameToCabal (unLoc ideclName)
    ideclAs' = ghcModuleNameToCabal . unLoc <$> ideclAs

ieToOccNames :: IE GhcPs -> [OccName]
ieToOccNames = \case
  IEVar _ (L _ x) _ -> [occName x]
  IEThingAbs _ (L _ x) _ -> [occName x]
  IEThingAll _ (L _ x) _ -> [occName x] -- TODO not quite correct, but how to do better?
  IEThingWith _ (L _ x) _ xs _ -> occName x : fmap (occName . unLoc) xs
  _ -> []

-- | Apply given module re-exports.
applyModuleReexports :: ModuleReexports -> [FixityImport] -> [FixityImport]
applyModuleReexports (ModuleReexports reexports) imports = imports >>= expand
  where
    expand i = do
      case Map.lookup (fimportModule i) reexports of
        Nothing -> pure i
        Just exports ->
          let exportToImport (mpackageName, mmodule) =
                i
                  { fimportPackage = mpackageName,
                    fimportModule = mmodule
                  }
           in NE.toList exports >>= expand . exportToImport

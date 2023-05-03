{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Definitions for fixity analysis.
module Ormolu.Fixity
  ( OpName,
    pattern OpName,
    unOpName,
    occOpName,
    FixityDirection (..),
    FixityInfo (..),
    defaultFixityInfo,
    FixityApproximation (..),
    defaultFixityApproximation,
    FixityOverrides (..),
    defaultFixityOverrides,
    ModuleReexports (..),
    defaultModuleReexports,
    PackageFixityMap (..),
    ModuleFixityMap (..),
    inferFixity,
    HackageInfo (..),
    hackageInfo,
    defaultDependencies,
    packageFixityMap,
    packageFixityMap',
    moduleFixityMap,
    applyFixityOverrides,
  )
where

import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.MemoTrie (memo)
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Language.Haskell.Syntax.ImpExp (ImportListInterpretation (..))
import Ormolu.Fixity.Imports (FixityImport (..))
import Ormolu.Fixity.Internal
#if BUNDLE_FIXITIES
import Data.FileEmbed (embedFile)
#else
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafePerformIO)
#endif

-- | The built-in 'HackageInfo' used by Ormolu.
hackageInfo :: HackageInfo
#if BUNDLE_FIXITIES
hackageInfo =
  Binary.runGet Binary.get $
    BL.fromStrict $(embedFile "extract-hackage-info/hackage-info.bin")
#else
-- The GHC WASM backend does not yet support Template Haskell, so we instead
-- pass in the encoded fixity DB via pre-initialization with Wizer.
hackageInfo =
  unsafePerformIO $
    Binary.runGet Binary.get . BL.fromStrict <$> B.readFile "hackage-info.bin"
{-# NOINLINE hackageInfo #-}
#endif

-- | Default set of packages to assume as dependencies e.g. when no Cabal
-- file is found or taken into consideration.
defaultDependencies :: Set PackageName
defaultDependencies = Set.singleton (mkPackageName "base")

-- | Compute the fixity map that is specific to the package we are formatting.
packageFixityMap ::
  -- | Set of packages to select
  Set PackageName ->
  -- | Package fixity map
  PackageFixityMap
packageFixityMap = packageFixityMap' hackageInfo

-- | The same as 'packageFixityMap', except this specific version of the
-- function allows the user to specify 'HackageInfo' used to build the final
-- fixity map.
packageFixityMap' ::
  -- | Hackage info
  HackageInfo ->
  -- | Set of packages to select
  Set PackageName ->
  -- | Package fixity map
  PackageFixityMap
packageFixityMap' (HackageInfo m) = memoSet $ \dependencies ->
  -- The core idea here is to transform:
  --
  -- Map PackageName (Map ModuleName (Map OpName FixityInfo))
  --
  -- into
  --
  -- Map OpName [(PackageName, ModuleName, FixityInfo)]
  --
  -- which we accomplish by turning 'Map's into tuples with 'Map.toList' and
  -- then flattening them with 'flatten :: [(a, [b])] -> [(a, b)]'.
  --
  -- The target type results from the need to be able to quickly index by
  -- the operator name when we do fixity resolution later.
  PackageFixityMap
    . Map.mapMaybe NE.nonEmpty
    . Map.fromListWith (<>)
    . fmap rearrange
    . flatten
    . Map.toList
    . Map.map (flatten . Map.toList . Map.map Map.toList)
    $ Map.restrictKeys m dependencies
  where
    rearrange (packageName, (moduleName, (opName, fixityInfo))) =
      (opName, [(packageName, moduleName, fixityInfo)])
    flatten xs = do
      (k, vs) <- xs
      v <- vs
      return (k, v)

-- | Compute the fixity map that is specific to the module we are formatting.
moduleFixityMap ::
  -- | Fixity information selected from dependencies of this package
  PackageFixityMap ->
  -- | A simplified representation of the import list in this module
  [FixityImport] ->
  -- | Fixity map specific to this module
  ModuleFixityMap
moduleFixityMap (PackageFixityMap m) imports =
  ModuleFixityMap $
    Map.insert
      ":"
      (Given colonFixityInfo)
      (Map.map FromModuleImports (Map.mapMaybeWithKey select m))
  where
    select ::
      OpName ->
      NonEmpty (PackageName, ModuleName, FixityInfo) ->
      Maybe (NonEmpty (FixityQualification, FixityInfo))
    select opName =
      let f (packageName, moduleName, fixityInfo) =
            (,fixityInfo)
              <$> resolveThroughImports packageName moduleName opName
       in NE.nonEmpty . concatMap f
    resolveThroughImports ::
      PackageName ->
      ModuleName ->
      OpName ->
      [FixityQualification]
    resolveThroughImports packageName moduleName opName =
      let doesImportMatch FixityImport {..} =
            let packageMatches =
                  case fimportPackage of
                    Nothing -> True
                    Just p -> p == packageName
                moduleMatches =
                  fimportModule == moduleName
                opMatches = case fimportList of
                  Nothing -> True
                  Just (Exactly, xs) -> opName `elem` xs
                  Just (EverythingBut, xs) -> opName `notElem` xs
             in packageMatches && moduleMatches && opMatches
       in fimportQualified <$> filter doesImportMatch imports

-- | Apply fixity overrides.
applyFixityOverrides ::
  -- | User overrides
  FixityOverrides ->
  -- | Module fixity map
  ModuleFixityMap ->
  -- | Module fixity map with overrides applied
  ModuleFixityMap
applyFixityOverrides (FixityOverrides o) (ModuleFixityMap m) =
  ModuleFixityMap (Map.union (Map.map Given o) m)

memoSet :: (Set PackageName -> v) -> Set PackageName -> v
memoSet f =
  memo (f . Set.fromAscList . fmap mkPackageName)
    . fmap unPackageName
    . Set.toAscList

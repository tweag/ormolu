{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Authoritative extraction of operator fixity information from the
-- sources of a curated set of packages.
--
-- The Hoogle database no longer records operators under the umbrella modules
-- that re-export them, and in general it is a lossy, Haddock-processed view
-- of a package. For a small set of popular, important packages we parse their
-- sources directly with @ghc-lib-parser@ (via Ormolu's own parser) and
-- extract:
--
--     * fixity declarations (@infixr 4 :>@), giving the authoritative fixity
--       of every operator a module defines;
--
--     * export lists, distinguishing operators exported by explicit name from
--       whole-module re-exports (@module M@);
--
--     * per-module import origins, so that an operator re-exported by explicit
--       name (e.g. @Control.Lens.Lens@ re-exporting @(&)@, which is imported
--       from @Data.Function@) can be traced to its defining module.
--
-- Re-exports are NOT resolved here. Instead this module returns the raw facts
-- ('PackageSource'); resolution against the fully merged database (Hoogle plus
-- all source parses) happens as a final global pass in "Main", so that a
-- re-exported operator resolves regardless of whether its fixity ultimately
-- comes from Hoogle or from another parsed package, and independently of the
-- order in which packages are processed.
module SourceExtract
  ( PackageSource (..),
    ModuleSource (..),
    Export (..),
    extractPackage,
  )
where

import Control.Exception (SomeException, try)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T.Utf8
import Distribution.ModuleName (ModuleName)
import GHC.Hs hiding (ModuleName, OpName)
import GHC.Types.Fixity qualified as GHC
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Ormolu.Config (Config (..), RegionDeltas (..), defaultConfig)
import Ormolu.Fixity
  ( FixityDirection (..),
    FixityInfo (..),
    OpName,
    PackageFixityMap (..),
    occOpName,
  )
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result (ParseResult (..), SourceSnippet (..))
import Ormolu.Utils (ghcModuleNameToCabal)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

-- | The raw facts extracted from a single parsed module.
data ModuleSource = ModuleSource
  { -- | Operator fixities declared in this module.
    msFixities :: Map OpName FixityInfo,
    -- | Operators this module exports and the module each is (re-)exported
    -- from, as far as can be told from this module alone.
    msExports :: [Export],
    -- | Whole-module re-exports: modules whose entire contents this module
    -- re-exports (@module M@ in the export list).
    msModuleReexports :: [ModuleName]
  }

-- | A single operator export from a module.
data Export = Export
  { -- | The operator being exported.
    exOp :: OpName,
    -- | The module the operator originates from: the module itself if it is
    -- defined or has a fixity declaration here, otherwise the module it was
    -- imported from (if that can be determined), otherwise 'Nothing'.
    exOrigin :: Maybe ModuleName
  }

-- | All parsed modules of a package, keyed by module name.
newtype PackageSource = PackageSource
  { unPackageSource :: Map ModuleName ModuleSource
  }

-- | Recursively list all Haskell source files under a directory.
listHaskellFiles :: FilePath -> IO [FilePath]
listHaskellFiles top = do
  entries <- listDirectory top
  fmap concat . mapM go $ map (top </>) entries
  where
    go path =
      doesDirectoryExist path >>= \case
        True -> listHaskellFiles path
        False -> pure [path | takeExtension path `elem` [".hs", ".hsig"]]

-- | @'Config' 'RegionDeltas'@ covering the whole file.
wholeFileConfig :: Config RegionDeltas
wholeFileConfig = defaultConfig <&> const (RegionDeltas 0 0)

-- | Parse a single module and summarize it. Read or parse failures yield
-- 'Nothing' rather than aborting, since we are best-effort scanning real
-- Hackage sources (CPP, exotic extensions, etc.).
parseModuleSource :: FilePath -> IO (Maybe (ModuleName, ModuleSource))
parseModuleSource path = do
  readResult <- try @SomeException (T.Utf8.readFile path)
  case readResult of
    Left _ -> pure Nothing
    Right rawInput -> do
      -- Strip CPP directive lines so CPP-using modules parse as a single
      -- unit; we keep the guarded code (operators are defined regardless of
      -- the CPP branch), avoiding Ormolu's CPP-region splitting that can drop
      -- declarations.
      let cleaned = dropCppDirectives rawInput
      parsed <-
        try @SomeException
          (parseModule wholeFileConfig (PackageFixityMap mempty) path cleaned)
      pure $ case parsed of
        Right (_, Right snippets)
          | (r : _) <- mapMaybe summarize snippets -> Just r
        _ -> Nothing

summarize :: SourceSnippet -> Maybe (ModuleName, ModuleSource)
summarize = \case
  RawSnippet _ -> Nothing
  ParsedSnippet ParseResult {prParsedSource = hsModule} ->
    case hsmodName hsModule of
      Nothing -> Nothing
      Just lname ->
        let fixities = collectFixities hsModule
            importOrigins = collectImportOrigins hsModule
            (exports, moduleReexports) = collectExports fixities importOrigins hsModule
         in Just
              ( ghcModuleNameToCabal (unLoc lname),
                ModuleSource
                  { msFixities = fixities,
                    msExports = exports,
                    msModuleReexports = moduleReexports
                  }
              )

-- | Collect fixity declarations from a module's top-level declarations.
collectFixities :: HsModule GhcPs -> Map OpName FixityInfo
collectFixities hsModule =
  Map.fromList
    [ (opNameFromRdr (unLoc lname), fixityInfo dir prec)
    | L _ (SigD _ (FixSig _ (FixitySig _ names (GHC.Fixity prec dir)))) <-
        hsmodDecls hsModule,
      lname <- names
    ]

-- | Map from an imported operator to the module it was imported from. Only
-- imports with an explicit import list (@import M (.., (&), ..)@) contribute,
-- since those unambiguously name the origin module of each listed operator.
collectImportOrigins :: HsModule GhcPs -> Map OpName ModuleName
collectImportOrigins hsModule =
  Map.fromList
    [ (op, ghcModuleNameToCabal (unLoc (ideclName decl)))
    | L _ decl <- hsmodImports hsModule,
      Just (_hiding, L _ ies) <- [ideclImportList decl],
      L _ ie <- ies,
      op <- opNamesFromIE ie
    ]

-- | Collect the operators a module exports, along with whole-module
-- re-exports. For each exported operator we record its origin: the module
-- itself if it has a local fixity declaration, else the module it was
-- imported from (if known).
collectExports ::
  Map OpName FixityInfo ->
  Map OpName ModuleName ->
  HsModule GhcPs ->
  ([Export], [ModuleName])
collectExports fixities importOrigins hsModule =
  case hsmodExports hsModule of
    -- No explicit export list: the module exports everything it defines. We
    -- only know about the operators it declares fixities for.
    Nothing ->
      ( [Export op (Just selfOrigin) | op <- Map.keys fixities],
        []
      )
      where
        selfOrigin = selfName
    Just (L _ ies) ->
      ( [ Export op (originOf op)
        | L _ ie <- ies,
          op <- opNamesFromIE ie
        ],
        [ ghcModuleNameToCabal (unLoc lmod)
        | L _ (IEModuleContents _ lmod) <- ies
        ]
      )
  where
    selfName = maybe (error "module without name") (ghcModuleNameToCabal . unLoc) (hsmodName hsModule)
    originOf op
      | Map.member op fixities = Just selfName
      | otherwise = Map.lookup op importOrigins

-- | Parse a whole package directory into its raw source facts.
extractPackage :: FilePath -> IO PackageSource
extractPackage root = do
  files <- listHaskellFiles root
  results <- traverse parseModuleSource files
  pure (PackageSource (Map.fromList (mapMaybe id results)))

-- | Extract operator names from an export/import item (both value and type
-- operator forms).
opNamesFromIE :: IE GhcPs -> [OpName]
opNamesFromIE = map opNameFromOcc . ieOccNames
  where
    ieOccNames = \case
      IEVar _ n _ -> wn (unLoc n)
      IEThingAbs _ n _ -> wn (unLoc n)
      IEThingAll _ n _ -> wn (unLoc n)
      IEThingWith _ n _ _ _ -> wn (unLoc n)
      _ -> []
    wn = \case
      IEName _ x -> [rdrNameOcc (unLoc x)]
      IEType _ x -> [rdrNameOcc (unLoc x)]
      _ -> []

opNameFromRdr :: RdrName -> OpName
opNameFromRdr = occOpName . rdrNameOcc

opNameFromOcc :: OccName -> OpName
opNameFromOcc = occOpName

fixityInfo :: GHC.FixityDirection -> Int -> FixityInfo
fixityInfo dir prec =
  FixityInfo
    { fiDirection = convertDir dir,
      fiPrecedence = fromIntegral prec
    }
  where
    convertDir = \case
      GHC.InfixL -> InfixL
      GHC.InfixR -> InfixR
      GHC.InfixN -> InfixN

dropCppDirectives :: T.Text -> T.Text
dropCppDirectives =
  T.unlines . map blankIfDirective . T.lines
  where
    blankIfDirective line
      | "#" `T.isPrefixOf` T.stripStart line = ""
      | otherwise = line

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Aeson qualified as A
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.Binary.Put qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.IO.Utf8 qualified as T.Utf8
import Distribution.ModuleName (ModuleName)
import Distribution.ModuleName qualified as ModuleName
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.PackageName qualified as PackageName
import Formatting
import Hoogle qualified
import Options.Applicative
import Ormolu.Fixity
import SourceExtract
  ( Export (..),
    ModuleSource (..),
    PackageSource (..),
    extractPackage,
  )
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)
import Text.Megaparsec.Error (errorBundlePretty)

defaultOutputPath :: FilePath
defaultOutputPath = "hackage-info.bin"

-- | Contains the database being constructed during the processing of Hoogle
-- files.
newtype State = State
  { -- | Hackage info
    sHackageInfo :: Map PackageName (Map ModuleName (Map OpName [FixityInfo]))
  }
  deriving (Eq, Show)

-- | Recursively list all files inside directory.
walkDir ::
  -- | Path to the root directory
  FilePath ->
  IO [FilePath]
walkDir top = do
  ds <- listDirectory top
  paths <- forM ds $ \d -> do
    let path = top </> d
    doesDirectoryExist path >>= \case
      True -> walkDir path
      False -> return [path]
  return (concat paths)

-- | Try to read the specified file using utf-8 encoding first, and latin1
-- otherwise.
readFileUtf8Latin1 :: FilePath -> IO Text
readFileUtf8Latin1 filePath = catch @UnicodeException (T.Utf8.readFile filePath) $
  \e -> do
    hprintLn
      stderr
      ("Unable to read " % string % " with UTF-8 (" % shown % "), trying latin1 encoding...")
      filePath
      e
    decodeLatin1 <$> ByteString.readFile filePath

-- | Scrap all fixity data from a Hoogle file, and update the state
-- accordingly.
extractFixitiesFromFile ::
  -- | Previous state
  State ->
  -- | Path of the Hoogle file to process
  FilePath ->
  -- | Updated state
  IO State
extractFixitiesFromFile state filePath = do
  fileContent <- readFileUtf8Latin1 filePath
  case Hoogle.parsePackage filePath fileContent of
    Left errorBundle -> do
      hPutStrLn stderr (errorBundlePretty errorBundle)
      exitWith (ExitFailure 1)
    Right (Hoogle.Package packageName modules) ->
      return $
        let handleModule st (Hoogle.Module moduleName decls) =
              let onDecl = \case
                    Hoogle.Symbol opName ->
                      registerOp packageName moduleName opName Nothing
                    Hoogle.Fixity opName fixityInfo ->
                      registerOp packageName moduleName opName (Just fixityInfo)
               in foldl' (flip onDecl) st decls
         in foldl' handleModule state modules

-- | Add fixity info for an operator.
registerOp ::
  -- | Name of the package in which the symbol declaration was found
  PackageName ->
  -- | Name of the module in which the symbol declaration was found
  ModuleName ->
  -- | Symbol name extracted from the symbol declaration in the Hoogle file
  OpName ->
  -- | Fixity info, if available
  Maybe FixityInfo ->
  -- | Current state
  State ->
  -- | Updated state
  State
registerOp packageName moduleName opName fixityInfo state@State {..} =
  let fixityInfoList = maybeToList fixityInfo
      sHackageInfo' = Map.alter alterPackage packageName sHackageInfo
      alterPackage = \case
        Nothing ->
          Just (Map.singleton moduleName (Map.singleton opName fixityInfoList))
        Just pkg -> Just (Map.alter alterModule moduleName pkg)
      alterModule = \case
        Nothing -> Just (Map.singleton opName fixityInfoList)
        Just mdl -> Just (Map.alter alterOp opName mdl)
      alterOp = \case
        Nothing -> Just fixityInfoList
        Just finfos -> Just (fixityInfoList ++ finfos)
   in state {sHackageInfo = sHackageInfo'}

-- | Build the final operator map.
finalizePackageToOps ::
  Map PackageName (Map ModuleName (Map OpName [FixityInfo])) ->
  Map PackageName (Map ModuleName (Map OpName FixityInfo))
finalizePackageToOps = Map.map (Map.map (Map.map finalize))
  where
    finalize = \case
      [] -> defaultFixityInfo
      -- In some very rare and exceptional cases there seem to be multiple
      -- conflicting fixity definitions. I think it is acceptable to be
      -- somewhat arbitrary in that case.
      (x : _) -> x

-- | Process the whole Hoogle database and return a map associating each
-- package name to its fixity map.
extractHoogleInfo ::
  -- | Path to the Hoogle directory containing all package directories
  FilePath ->
  IO (Map PackageName (Map ModuleName (Map OpName FixityInfo)))
extractHoogleInfo hoogleDatabasePath = do
  hoogleFiles <- walkDir hoogleDatabasePath
  State {..} <-
    foldM
      extractFixitiesFromFile
      (State Map.empty)
      hoogleFiles
  let sHackageInfoFinalized = finalizePackageToOps sHackageInfo
  displayFixityStats sHackageInfoFinalized
  return sHackageInfoFinalized

-- | Display stats about the Hoogle database processing.
displayFixityStats ::
  Map PackageName (Map ModuleName (Map OpName FixityInfo)) ->
  IO ()
displayFixityStats packages =
  hprintLn
    stdout
    ( "Found "
        % int
        % " operator declarations across "
        % int
        % " packages"
    )
    declarationCount
    packageCount
  where
    packageCount = Map.size packages
    modulesPerPackage = Map.elems packages
    declarationsPerModule = concatMap Map.elems modulesPerPackage
    declarationCount = sum (Map.size <$> declarationsPerModule)

-- ToJSON orphan instances

deriving anyclass instance A.ToJSON FixityInfo

deriving anyclass instance A.ToJSON FixityDirection

instance A.ToJSON OpName where
  toJSON = A.toJSON . unOpName

deriving anyclass instance A.ToJSONKey OpName

instance A.ToJSON ModuleName where
  toJSON = A.toJSON . ModuleName.toFilePath

deriving anyclass instance A.ToJSONKey ModuleName

instance A.ToJSON PackageName where
  toJSON = A.toJSON . PackageName.unPackageName

deriving anyclass instance A.ToJSONKey PackageName

-- CLI config

data Config
  = Generate
      { cfgHoogleDatabasePath :: FilePath,
        cfgPackagesSourcePath :: Maybe FilePath,
        cfgOutputPath :: FilePath
      }
  | Dump
      { cfgPath :: FilePath
      }
  deriving (Eq, Show)

configParserInfo :: ParserInfo Config
configParserInfo = info (helper <*> configParser) fullDesc
  where
    configParser :: Parser Config
    configParser =
      subparser . mconcat $
        [ command "generate" . info (helper <*> generateParser) $
            fullDesc <> progDesc "Generate a Hackage info database.",
          command "dump" . info (helper <*> dumpParser) $
            fullDesc <> progDesc "Dump a generated Hackage info database to JSON."
        ]

    generateParser :: Parser Config
    generateParser =
      Generate
        <$> (strArgument . mconcat)
          [ metavar "HOOGLE_DATABASE_PATH",
            help
              "Download: mkdir -p hoogle-database && \
              \curl https://hackage.haskell.org/packages/hoogle.tar.gz | \
              \tar -xz -C hoogle-database"
          ]
        <*> optional
          ( (strOption . mconcat)
              [ long "packages-source-path",
                metavar "PACKAGES_SOURCE_PATH",
                help
                  "Directory with unpacked sources of the important \
                  \packages, used to recover whole-module re-exports."
              ]
          )
        <*> (strOption . mconcat)
          [ short 'o',
            long "output-path",
            metavar "OUTPUT_PATH",
            value defaultOutputPath
          ]

    dumpParser :: Parser Config
    dumpParser =
      Dump
        <$> (strArgument . mconcat)
          [ metavar "HACKAGE_INFO_PATH",
            help "A generated Hackage info database"
          ]

main :: IO ()
main =
  execParser configParserInfo >>= \case
    Generate {..} -> do
      hackageInfo' <- extractHoogleInfo cfgHoogleDatabasePath
      sources <- case cfgPackagesSourcePath of
        Nothing -> pure Map.empty
        Just path -> extractSourcePackages path
      let merged = mergeSourceFixities sources hackageInfo'
          final = resolveReexports sources merged
      BL.writeFile cfgOutputPath . Binary.runPut . Binary.put $
        HackageInfo final
    Dump {..} -> do
      HackageInfo hackageInfo' <-
        Binary.runGet Binary.get <$> BL.readFile cfgPath
      BL.putStr $ A.encode hackageInfo'

-- | Scan the packages-source directory, parsing every package found there.
-- The set of packages to parse is chosen by the caller (see
-- @extract-hackage-info.sh@), which populates this directory; we parse
-- whatever it contains. The directory is expected to hold one subdirectory
-- per package, named @\<package\>-\<version\>@ (as produced by unpacking a
-- Hackage tarball).
extractSourcePackages ::
  FilePath ->
  IO (Map PackageName PackageSource)
extractSourcePackages root = do
  subdirs <- listDirectory root >>= filterM (doesDirectoryExist . (root </>))
  fmap (Map.fromList . catMaybes) . forM subdirs $ \subdir -> do
    case packageNameOf subdir of
      Nothing -> pure Nothing
      Just packageName -> do
        source <- extractPackage (root </> subdir)
        hprintLn
          stdout
          ("Parsed " % int % " modules from " % string)
          (Map.size (unPackageSource source))
          subdir
        pure (Just (packageName, source))
  where
    packageNameOf subdir =
      case reverse (splitOn '-' subdir) of
        (_version : nameParts@(_ : _)) ->
          Just (PackageName.mkPackageName (intercalate "-" (reverse nameParts)))
        _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn c = foldr step [[]]
  where
    step x acc@(cur : rest)
      | x == c = [] : acc
      | otherwise = (x : cur) : rest
    step _ [] = [[]]

-- | Merge the locally-declared fixities of each source-parsed module over
-- the Hoogle-derived database. Source data wins for operators a module
-- actually declares a fixity for. This does not yet resolve re-exports.
mergeSourceFixities ::
  Map PackageName PackageSource ->
  Map PackageName (Map ModuleName (Map OpName FixityInfo)) ->
  Map PackageName (Map ModuleName (Map OpName FixityInfo))
mergeSourceFixities sources = Map.unionWith mergePackage sourceFixities
  where
    sourceFixities =
      Map.map
        (Map.map msFixities . unPackageSource)
        sources
    mergePackage = Map.unionWith Map.union

-- | Resolve all re-export edges against the fully merged database, as a global
-- final pass. Two kinds of edges, both keyed by the re-exporting module and
-- resolved against the whole database (so origins may live in Hoogle-only
-- packages, e.g. @base@'s @Data.Function@, or in other parsed packages):
--
--   * whole-module: module @A@ re-exports all of module @B@; @A@ gains every
--     operator @B@ has;
--
--   * explicit-name: module @A@ re-exports operator @op@ originating in module
--     @B@; @A@ gains @op@ with @B@'s fixity for it.
--
-- Resolution iterates to a fixed point, so multi-hop chains are covered
-- regardless of processing order.
--
-- Precedence: a module's own source-declared fixity always wins; otherwise
-- a resolved re-export wins over any pre-existing (Hoogle-derived) entry,
-- since for important packages the Hoogle data is exactly what we distrust.
resolveReexports ::
  Map PackageName PackageSource ->
  Map PackageName (Map ModuleName (Map OpName FixityInfo)) ->
  Map PackageName (Map ModuleName (Map OpName FixityInfo))
resolveReexports sources db0 = fixpoint db0
  where
    -- Operators each module declares in its own source; these are never
    -- overwritten by re-export resolution.
    protected :: Map PackageName (Map ModuleName (Set OpName))
    protected =
      Map.map
        (Map.map (Map.keysSet . msFixities) . unPackageSource)
        sources
    isProtected pkg modName op =
      maybe False (Set.member op) $
        Map.lookup modName =<< Map.lookup pkg protected
    -- A global module -> op -> fixity view, used to look up origins across
    -- all packages regardless of which one defines a module.
    globalModules db =
      Map.unionsWith Map.union (Map.elems db)

    -- All re-export edges gathered from the parsed sources, tagged with the
    -- package the re-exporting module belongs to.
    wholeModuleEdges :: [(PackageName, ModuleName, ModuleName)]
    wholeModuleEdges =
      [ (pkg, reexporter, origin)
      | (pkg, PackageSource mods) <- Map.toList sources,
        (reexporter, ms) <- Map.toList mods,
        origin <- msModuleReexports ms
      ]

    explicitEdges :: [(PackageName, ModuleName, OpName, ModuleName)]
    explicitEdges =
      [ (pkg, reexporter, exOp e, origin)
      | (pkg, PackageSource mods) <- Map.toList sources,
        (reexporter, ms) <- Map.toList mods,
        e <- msExports ms,
        Just origin <- [exOrigin e]
      ]

    fixpoint db =
      let g = globalModules db
          db' = foldr (applyExplicit g) (foldr (applyWhole g) db wholeModuleEdges) explicitEdges
       in if db' == db then db else fixpoint db'

    applyWhole g (pkg, reexporter, origin) db =
      case Map.lookup origin g of
        Nothing -> db
        Just originOps -> addOps pkg reexporter originOps db

    applyExplicit g (pkg, reexporter, op, origin) db =
      case Map.lookup op =<< Map.lookup origin g of
        Nothing -> db
        Just fixity -> addOps pkg reexporter (Map.singleton op fixity) db

    -- Add re-exported operators to a module within a package.
    -- Source-declared operators (protected) are kept; all other operators
    -- are overwritten by the re-exported fixity.
    addOps ::
      PackageName ->
      ModuleName ->
      Map OpName FixityInfo ->
      Map PackageName (Map ModuleName (Map OpName FixityInfo)) ->
      Map PackageName (Map ModuleName (Map OpName FixityInfo))
    addOps pkg modName ops db =
      Map.alter (Just . updatePackage) pkg db
      where
        updatePackage = Map.alter (Just . updateModule) modName . orEmpty
        updateModule existing =
          Map.foldrWithKey insertOp (orEmpty existing) ops
        insertOp op fixity acc
          | isProtected pkg modName op = acc -- keep source-declared fixity
          | otherwise = Map.insert op fixity acc
        orEmpty = maybe Map.empty id

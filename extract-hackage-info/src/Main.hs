{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Binary qualified as Binary
import Data.Binary.Put qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as BL
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Text.IO qualified as TIO
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName (PackageName)
import Formatting
import Hoogle qualified
import Options.Applicative
import Ormolu.Fixity
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
readFileUtf8Latin1 filePath = catch @IOException (TIO.readFile filePath) $
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

data Config = Config
  { cfgHoogleDatabasePath :: FilePath,
    cfgOutputPath :: FilePath
  }
  deriving (Eq, Show)

configParserInfo :: ParserInfo Config
configParserInfo = info (helper <*> configParser) fullDesc
  where
    configParser :: Parser Config
    configParser =
      Config
        <$> (strArgument . mconcat)
          [ metavar "HOOGLE_DATABASE_PATH",
            help
              "Download: mkdir -p hoogle-database && \
              \curl https://hackage.haskell.org/packages/hoogle.tar.gz | \
              \tar -xz -C hoogle-database"
          ]
        <*> (strOption . mconcat)
          [ short 'o',
            long "output-path",
            metavar "OUTPUT_PATH",
            value defaultOutputPath
          ]

main :: IO ()
main = do
  Config {..} <- execParser configParserInfo
  hackageInfo' <- extractHoogleInfo cfgHoogleDatabasePath
  BL.writeFile cfgOutputPath . Binary.runPut . Binary.put $
    HackageInfo hackageInfo'

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Formatting
import Options.Applicative
import Ormolu.Fixity hiding (packageToOps, packageToPopularity)
import Ormolu.Fixity.Parser
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (makeRelative, splitPath, (</>))
import System.IO (stderr, stdout)
import Text.HTML.TagSoup (Tag (TagText), parseTags)
import Text.HTML.TagSoup.Match (tagCloseLit, tagOpenLit)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

defaultOutputPath :: FilePath
defaultOutputPath = "extract-hackage-info/hackage-info.bin"

-- | This fixity info is used when we find an operator declaration in a
-- package, but no matching fixity declaration.
unspecifiedFixityInfo :: FixityInfo
unspecifiedFixityInfo = FixityInfo (Just InfixL) 9 9

-- | Contains the database being constructed during the processing of Hoogle
-- files.
data State = State
  { -- | packageName -map-> (operatorName -map-> fixityDefinitions)
    -- we keep a list of fixity definitions for each pair
    -- (packageName, operatorName) because sometimes a package itself has
    -- conflicting fixity declarations for a same operator
    -- (called self-conflicts), and we want to emit a warning message later
    -- for these
    sPackageToOps :: Map PackageName (Map OpName [FixityInfo]),
    -- | How many Hoogle files have been processed
    sProcessedFiles :: Int
  }
  deriving (Eq)

-- | Exit with an error message.
exitWithMsg :: Text -> IO ()
exitWithMsg t = do
  TIO.hPutStrLn stderr t
  exitWith (ExitFailure 1)

showT :: (Show a) => a -> Text
showT = T.pack . show

readT :: (Read a) => Text -> a
readT = read . T.unpack

indentLines :: [Text] -> [Text]
indentLines = fmap ("  " <>)

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

-- | Extract the package name from a path to a Hoogle file.
getPackageName ::
  -- | Path to the Hoogle directory containing all package directories
  FilePath ->
  -- | Path to the Hoogle file
  FilePath ->
  -- | Package name extracted from the Hoogle file
  IO PackageName
getPackageName rootPath filePath = do
  unless (rootPath `isPrefixOf` filePath) $
    exitWithMsg $
      sformat (string % " does not start with " % string) rootPath filePath
  let packageName =
        stripSuffix' "/" $
          T.pack . head . splitPath $
            makeRelative rootPath filePath
      stripSuffix' suffix txt = fromMaybe txt $ T.stripSuffix suffix txt
  when (T.null packageName) $
    exitWithMsg $
      sformat
        ("Extracted package name is empty for " % string % " (base path = " % string % ")")
        filePath
        rootPath
  pure . mkPackageName . T.unpack $ packageName

-- | Try to read the specified file using utf-8 encoding first,
-- and latin1 otherwise.
readFileUtf8Latin1 :: FilePath -> IO Text
readFileUtf8Latin1 filePath = catch @IOException (TIO.readFile filePath) $
  \e -> do
    hprintLn
      stderr
      ("Unable to read " % string % " with UTF-8 (" % shown % "), trying latin1 encoding...")
      filePath
      e
    decodeLatin1 <$> ByteString.readFile filePath

-- | When a symbol declaration is encountered,
-- e.g. @(+) :: Num a => a -> a -> a@, update the fixity map accordingly.
onSymbolDecl ::
  -- | Name of the package in which the symbol declaration was found
  PackageName ->
  -- | Symbol name extracted from the symbol declaration in the Hoogle file
  OpName ->
  -- | Current state
  State ->
  -- | Updated state
  State
onSymbolDecl packageName declOpName state@State {..} =
  let sPackageToOps' = case Map.lookup packageName sPackageToOps of
        Nothing ->
          Map.insert
            packageName
            (Map.singleton declOpName [])
            sPackageToOps
        Just packageFixityMap ->
          case Map.lookup declOpName packageFixityMap of
            Nothing ->
              Map.insert
                packageName
                (Map.insert declOpName [] packageFixityMap)
                sPackageToOps
            Just _ -> sPackageToOps
   in state {sPackageToOps = sPackageToOps'}

-- | When a fixity declaration is encountered, e.g. @infixr 5 :@, update the
-- fixity map accordingly.
onFixityDecl ::
  -- | Name of the package in which the symbol declaration was found
  PackageName ->
  -- | Tuple of operator name and fixity info
  (OpName, FixityInfo) ->
  -- | Current state
  State ->
  -- | Updated state
  State
onFixityDecl packageName (opName, fixDecl) state@State {..} =
  let sPackageToOps' = case Map.lookup packageName sPackageToOps of
        Nothing ->
          Map.insert
            packageName
            (Map.singleton opName [fixDecl])
            sPackageToOps
        Just packageFixityMap ->
          case fromMaybe [] $ Map.lookup opName packageFixityMap of
            fixDecls
              | fixDecl `elem` fixDecls ->
                  sPackageToOps
            fixDecls ->
              Map.insert
                packageName
                ( Map.insert
                    opName
                    (fixDecl : fixDecls)
                    packageFixityMap
                )
                sPackageToOps
   in state {sPackageToOps = sPackageToOps'}

-- | Represent an operator for which we found conflicting definitions
-- originating from the same package.
data SelfConflict = SelfConflict
  { scPackageName :: PackageName,
    scOperatorName :: OpName,
    scConflictingDefs :: [FixityInfo]
  }

-- | From a map allowing self conflicts, build the final map
-- packageName -map-> (operatorName -map-> fixityInfo)
-- (where conflicting definitions from self-conflicts are merged), and also
-- return the list of self-conflicts
finalizePackageToOps ::
  Map PackageName (Map OpName [FixityInfo]) ->
  (Map PackageName (Map OpName FixityInfo), [SelfConflict])
finalizePackageToOps hashmap =
  ( Map.map (Map.map finalize) hashmap,
    concatMap injectFst
      . Map.toList
      . Map.map (Map.toList . Map.filter hasConflict)
      $ hashmap
  )
  where
    finalize = \case
      [] -> unspecifiedFixityInfo
      fs -> sconcat . NE.fromList $ fs
    hasConflict = (> 1) . length
    injectFst (packageName, opFixs) =
      uncurry (SelfConflict packageName) <$> opFixs

-- | Scrap all fixity data from a Hoogle file, and update the state
-- accordingly.
extractFixitiesFromFile ::
  -- | Path to the Hoogle directory containing all package directories
  FilePath ->
  -- | Previous state
  State ->
  -- | Path of the Hoogle file to process
  FilePath ->
  -- | Updated state
  IO State
extractFixitiesFromFile
  hoogleDatabasePath
  state@State {sProcessedFiles}
  filePath = do
    fileContent <- liftIO . readFileUtf8Latin1 $ filePath
    packageName <- liftIO $ getPackageName hoogleDatabasePath filePath
    let onDecl (SymbolDecl opName) = onSymbolDecl packageName opName
        onDecl (FixityDecl opInfo) = onFixityDecl packageName opInfo
        state' = foldl' (flip onDecl) state $ parseDecls fileContent
    return state' {sProcessedFiles = sProcessedFiles + 1}

-- | The types of declarations in the Hoogle files we are interested in.
data DeclType
  = -- | See third argument of 'onSymbolDecl'.
    SymbolDecl OpName
  | -- | See third argument of 'onFixityDecl'.
    FixityDecl (OpName, FixityInfo)

-- | Parse all 'DeclType's in some file content.
parseDecls :: Text -> [DeclType]
parseDecls = either mempty id . MP.runParser pDecls ""
  where
    pDecls = mconcat <$> pDecl `MP.sepEndBy` MP.newline
    pDecl :: MP.Parsec Void Text [DeclType]
    pDecl =
      asum
        [ fmap FixityDecl <$> MP.try pFixity,
          pure . SymbolDecl <$> MP.try pSymbolDecl,
          [] <$ pRemainingLine
        ]
      where
        pRemainingLine = MP.takeWhileP Nothing (/= '\n')
        pSymbolDecl =
          MP.char '(' *> pOperator <* MP.chunk ") :: " <* pRemainingLine

-- | Process the whole Hoogle database and return a map associating each
-- package name to its fixity map.
extractHoogleInfo ::
  -- | Path to the hoogle directory containing all package directories
  FilePath ->
  IO (Map PackageName FixityMap)
extractHoogleInfo hoogleDatabasePath = do
  hoogleFiles <- walkDir hoogleDatabasePath
  State {..} <-
    foldM
      (extractFixitiesFromFile hoogleDatabasePath)
      (State {sPackageToOps = Map.empty, sProcessedFiles = 0})
      hoogleFiles
  hprintLn
    stdout
    (int % " Hoogle files processed!")
    sProcessedFiles
  let (packageToOps, selfConflicts) = finalizePackageToOps sPackageToOps
  displayFixityStats packageToOps
  displaySelfConflicts selfConflicts
  return packageToOps

-- | Warn the user about self-conflicts.
displaySelfConflicts :: [SelfConflict] -> IO ()
displaySelfConflicts selfConflicts =
  unless (null selfConflicts) $ do
    hprintLn
      stdout
      ("Found " % int % " conflicting declarations within packages themselves:")
      (length selfConflicts)
    TIO.putStrLn $ T.intercalate "\n" selfConflictLines
  where
    selfConflictLines = concat $ showSc <$> sortedSelfConflicts
    sortedSelfConflicts =
      sortBy
        ( \(SelfConflict p1 o1 _) (SelfConflict p2 o2 _) ->
            compare (p1, o1) (p2, o2)
        )
        selfConflicts
    showSc SelfConflict {scPackageName, scOperatorName, scConflictingDefs} =
      sformat
        ("(in " % string % ") " % string)
        (unPackageName scPackageName)
        (T.unpack $ unOpName scOperatorName)
        : indentLines (showT <$> scConflictingDefs)

-- | Display stats about the Hoogle database processing.
displayFixityStats :: Map PackageName FixityMap -> IO ()
displayFixityStats packageToOps =
  hprintLn
    stdout
    ( "Found "
        % int
        % " operator declarations across "
        % int
        % " packages for a total of "
        % int
        % " distinct operators"
    )
    declCount
    packagesCount
    distinctOpCount
  where
    packagesCount = Map.size packageToOps
    declCount = sum $ Map.size <$> fixityMaps
    distinctOpCount =
      Set.size . Set.fromList . concat $
        Map.keys <$> fixityMaps
    fixityMaps = Map.elems packageToOps

-- | Extract package download counts from the hackage HTML page.
extractHackageInfo ::
  -- | Path to the Hackage HTML page
  FilePath ->
  -- | Map packageName -> download count
  IO (Map PackageName Int)
extractHackageInfo filePath = do
  content <- TIO.readFile filePath
  let soup = filterBlankTags $ parseTags content
      tableBody =
        drop 7 $
          takeWhile (not . tagCloseLit "table") $
            dropWhile (not . tagOpenLit "table" (const True)) soup
      processRow tags = case extractText <$> groupOn "td" tags of
        rawName : rawDlCount : _ -> return $ Just (mkPackageName name, dlCount)
          where
            name = T.unpack . T.strip . head $ T.split (== ' ') rawName
            dlCount = readT $ T.strip rawDlCount :: Int
        _ -> do
          hprintLn
            stdout
            ("Invalid line: " % stext)
            (T.intercalate " " $ showT <$> tags)
          return Nothing
      extractText tags = T.intercalate "" $ extractText' <$> tags
      extractText' = \case
        TagText t -> t
        _ -> ""
      groupOn _ [] = []
      groupOn selector (_ : ts) =
        let (tags, remTags) = break (tagOpenLit selector (const True)) ts
         in init tags : groupOn selector remTags
      filterBlankTags =
        filter
          ( \case
              TagText t | isBlank t -> False
              _ -> True
          )
      isBlank t = null $ dropWhile (`elem` [' ', '\t', '\n']) (T.unpack t)
  result <- Map.fromList . catMaybes <$> traverse processRow (groupOn "tr" tableBody)
  hprintLn
    stdout
    ("Found popularity information for " % int % " packages")
    (Map.size result)
  return result

-- | Limit the number of items in a map.
limitMap :: (Ord k) => Int -> Map k v -> Map k v
limitMap n = Map.fromList . take n . Map.toList

data Config = Config
  { cfgHoogleDatabasePath :: FilePath,
    cfgHackageDatabasePath :: FilePath,
    cfgOutputPath :: FilePath,
    cfgDebugLimit :: Maybe Int
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
        <*> (strArgument . mconcat)
          [ metavar "HACKAGE_DATABASE_PATH",
            help
              "Download: curl https://hackage.haskell.org/packages/browse \
              \ -o hackage-database.html"
          ]
        <*> (strOption . mconcat)
          [ short 'o',
            long "output-path",
            metavar "OUTPUT_PATH",
            value defaultOutputPath
          ]
        <*> (option (Just <$> auto) . mconcat)
          [ short 'd',
            long "debug-limit",
            metavar "N",
            value Nothing
          ]

main :: IO ()
main = do
  Config {..} <- execParser configParserInfo
  packageToOps <- extractHoogleInfo cfgHoogleDatabasePath
  packageToPop <- extractHackageInfo cfgHackageDatabasePath
  let (packageToOps', packageToPop') = case cfgDebugLimit of
        Nothing -> (packageToOps, packageToPop)
        Just n ->
          ( limitMap n <$> limitMap n packageToOps,
            limitMap n packageToPop
          )
  BL.writeFile cfgOutputPath . Binary.runPut . Binary.put $
    HackageInfo packageToOps' packageToPop'

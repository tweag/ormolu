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
import Data.Aeson (encodeFile)
import qualified Data.ByteString as ByteString
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
import Formatting
import Options.Applicative
import Ormolu.Fixity hiding (packageToOps, packageToPopularity)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (makeRelative, splitPath, (</>))
import System.IO (stderr, stdout)
import Text.HTML.TagSoup (Tag (TagText), parseTags)
import Text.HTML.TagSoup.Match (tagCloseLit, tagOpenLit)
import Text.Regex.Pcre2 (capture, regex)

defaultOutputPath :: FilePath
defaultOutputPath = "extract-hackage-info/hackage-info.json"

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
    sPackageToOps :: Map String (Map String [FixityInfo]),
    -- | How many Hoogle files have been processed
    sProcessedFiles :: Int
  }
  deriving (Eq)

-- | Exit with an error message.
exitWithMsg :: Text -> IO ()
exitWithMsg t = do
  TIO.hPutStrLn stderr t
  exitWith (ExitFailure 1)

showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> a
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
  IO Text
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
  return packageName

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

-- | Extract the first element and last element from a list if possible, and
-- return the tuple (first, middle, last) where middle corresponds to all
-- the elements in between.
firstMiddleLast :: [a] -> Maybe (a, [a], a)
firstMiddleLast = \case
  x1 : x2 : xs -> Just (x1, init (x2 : xs), last (x2 : xs))
  _ -> Nothing

-- | Normalize a symbol name extracted from a symbol declaration to match
-- the one used later in the AST.
declToNormName :: String -> String
declToNormName declOpName = case firstMiddleLast declOpName of
  Just ('(', middle, ')') -> middle
  _ -> declOpName

-- | Normalize a symbol name extracted from a fixity declaration to match
-- the one used later in the AST.
infixToNormName :: String -> String
infixToNormName infixOpName = case firstMiddleLast infixOpName of
  Just ('`', middle, '`') -> middle
  _ -> infixOpName

-- | When a symbol declaration is encountered,
-- e.g. @(+) :: Num a => a -> a -> a@, update the fixity map accordingly.
onSymbolDecl ::
  -- | Name of the package in which the symbol declaration was found
  Text ->
  -- | Current state
  State ->
  -- | Symbol name extracted from the symbol declaration in the Hoogle file,
  -- before normalization
  Text ->
  -- | Updated state
  State
onSymbolDecl packageName state@State {..} declOpName =
  let sPackageToOps' = case Map.lookup packageName' sPackageToOps of
        Nothing ->
          Map.insert
            packageName'
            (Map.singleton normOpName [])
            sPackageToOps
        Just packageFixityMap ->
          case Map.lookup normOpName packageFixityMap of
            Nothing ->
              Map.insert
                packageName'
                (Map.insert normOpName [] packageFixityMap)
                sPackageToOps
            Just _ -> sPackageToOps
      normOpName = declToNormName . T.unpack $ declOpName
      packageName' = T.unpack packageName
   in state {sPackageToOps = sPackageToOps'}

-- | When a fixity declaration is encountered, e.g. @infixr 5 :@, update the
-- fixity map accordingly.
onFixityDecl ::
  -- | Name of the package in which the symbol declaration was found
  Text ->
  -- | Current state
  State ->
  -- | Tuple (fixity direction, precedence level, operator name);
  -- no item is normalized at this point
  (Text, Text, Text) ->
  -- | Updated state
  State
onFixityDecl packageName state@State {..} (rawFixDir, rawFixPrec, infixOpName) =
  let sPackageToOps' = case Map.lookup packageName' sPackageToOps of
        Nothing ->
          Map.insert
            packageName'
            (Map.singleton normOpName [fixDecl])
            sPackageToOps
        Just packageFixityMap ->
          case fromMaybe [] $ Map.lookup normOpName packageFixityMap of
            fixDecls
              | fixDecl `elem` fixDecls ->
                  sPackageToOps
            fixDecls ->
              Map.insert
                packageName'
                ( Map.insert
                    normOpName
                    (fixDecl : fixDecls)
                    packageFixityMap
                )
                sPackageToOps
      packageName' = T.unpack packageName
      normOpName = infixToNormName $ T.unpack infixOpName
      fixDecl =
        let fixPrec = readT rawFixPrec
         in FixityInfo
              { fiDirection = Just . readFixDir . T.unpack $ rawFixDir,
                fiMinPrecedence = fixPrec,
                fiMaxPrecedence = fixPrec
              }
      readFixDir = \case
        "infix" -> InfixN
        "infixr" -> InfixR
        "infixl" -> InfixL
        other -> error $ "unexpected fixity direction: " ++ other
   in state {sPackageToOps = sPackageToOps'}

-- | Represent an operator for which we found conflicting definitions
-- originating from the same package.
data SelfConflict = SelfConflict
  { scPackageName :: String,
    scOperatorName :: String,
    scConflictingDefs :: [FixityInfo]
  }

-- | From a map allowing self conflicts, build the final map
-- packageName -map-> (operatorName -map-> fixityInfo)
-- (where conflicting definitions from self-conflicts are merged), and also
-- return the list of self-conflicts
finalizePackageToOps ::
  Map String (Map String [FixityInfo]) ->
  (Map String (Map String FixityInfo), [SelfConflict])
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
    let state' =
          foldl' @[]
            (onSymbolDecl packageName)
            state
            (fromSymbolDecl <$> symbolDecls fileContent)
        state'' =
          foldl' @[]
            (onFixityDecl packageName)
            state'
            (concatMap @[] fromFixityDecl $ fixityDecls fileContent)
        fromSymbolDecl match = capture @"declOpName" match
        fromFixityDecl match =
          (capture @"fixDir" match,capture @"fixPrec" match,)
            <$> splitInfixOpNames (capture @"infixOpNames" match)
        splitInfixOpNames "" = []
        splitInfixOpNames s = case [regex|^(?:\s*?,\s*?)?(?<infixOpName>[^,\s]+)(?<remaining>.*)$|] s of
          [match] ->
            capture @"infixOpName" match
              : splitInfixOpNames (capture @"remaining" match)
          _ -> error $ "remaining text: " ++ T.unpack s

        symbolDecls = [regex|(?m)^\s*?(?<declOpName>\([^)]+?\))\s*?::.*$|]
        fixityDecls = [regex|(?m)^\s*?(?<fixDir>infix[rl]?)\s+?(?<fixPrec>[0-9])\s+?(?<infixOpNames>(?:[^,\s]+\s*?,\s*?)*?[^,\s]+)\s*$|]
    return state'' {sProcessedFiles = sProcessedFiles + 1}

-- | Process the whole Hoogle database and return a map associating each
-- package name to its fixity map.
extractHoogleInfo ::
  -- | Path to the hoogle directory containing all package directories
  FilePath ->
  IO (Map String FixityMap)
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
      ("Found" % int % " conflicting declarations within packages themselves:")
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
        scPackageName
        scOperatorName
        : indentLines (showT <$> scConflictingDefs)

-- | Display stats about the Hoogle database processing.
displayFixityStats :: Map String FixityMap -> IO ()
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
  IO (Map String Int)
extractHackageInfo filePath = do
  content <- TIO.readFile filePath
  let soup = filterBlankTags $ parseTags content
      tableBody =
        drop 7 $
          takeWhile (not . tagCloseLit "table") $
            dropWhile (not . tagOpenLit "table" (const True)) soup
      processRow tags = case extractText <$> groupOn "td" tags of
        rawName : rawDlCount : _ -> return $ Just (name, dlCount)
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
limitMap :: Ord k => Int -> Map k v -> Map k v
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
  encodeFile cfgOutputPath $
    HackageInfo packageToOps' packageToPop'

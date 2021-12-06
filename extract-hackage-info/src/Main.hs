{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Text.Format hiding (format)
import qualified Data.Text.Format as Format
import Data.Text.Format.Params (Params)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import GHC.Utils.Monad (mapMaybeM)
import Options.Applicative
import Ormolu.Fixity hiding (packageToOps, packageToPopularity)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (makeRelative, splitPath, (</>))
import System.IO (stderr)
import Text.HTML.TagSoup (Tag (TagText), parseTags)
import Text.HTML.TagSoup.Match (tagCloseLit, tagOpenLit)
import Text.Regex.Pcre2 (capture, regex)

defaultOutputPath :: FilePath
defaultOutputPath = "extract-hackage-info/hackage-info.json"

unspecifiedFixityInfo :: FixityInfo
unspecifiedFixityInfo = FixityInfo (Just InfixL) 9 9

initialState :: State
initialState =
  State
    { sPackageToOps = Map.empty,
      sProcessedFiles = 0
    }

data State = State
  { sPackageToOps :: Map String (Map String [FixityInfo]),
    sProcessedFiles :: Int
  }
  deriving (Eq)

-- | format using the Strict variant of Data.Text
format :: forall ps. Params ps => Format -> ps -> Text
format f p = TL.toStrict $ Format.format f p

-- | Put a formatted string
putFmtLn :: forall ps. Params ps => Format -> ps -> IO ()
putFmtLn f p = TIO.putStrLn $ format f p

-- | Exit with an error message
exitWithFmt :: forall ps. Params ps => Format -> ps -> IO ()
exitWithFmt f p = do
  TIO.hPutStrLn stderr $ format f p
  exitWith (ExitFailure 1)

showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> a
readT = read . T.unpack

indentLines :: [Text] -> [Text]
indentLines = fmap ("  " <>)

-- | Recursively list all files inside directory
walkDir ::
  -- | Path to the root directory
  FilePath ->
  -- | Whether we should exclude a file of the result list based on its path
  (FilePath -> Bool) ->
  IO [FilePath]
walkDir top exclude = do
  ds <- listDirectory top
  paths <- forM ds $ \d -> do
    let path = top </> d
    doesDirectoryExist path >>= \case
      True -> walkDir path exclude
      False -> return $ filter (not . exclude) [path]
  return (concat paths)

getPackageName ::
  -- | Root path
  FilePath ->
  -- | Current file path
  FilePath ->
  IO Text
getPackageName rootPath filePath = do
  when (not (rootPath `isPrefixOf` filePath)) $
    exitWithFmt
      "{} do not start with {}"
      (T.pack filePath, T.pack rootPath)
  let packageName =
        stripSuffix' "/" $
          T.pack . head . splitPath $
            makeRelative rootPath filePath
      stripSuffix' suffix txt = fromMaybe txt $ T.stripSuffix suffix txt
  when (T.null packageName) $
    exitWithFmt
      "Extracted package name is empty for {} (base path = {})"
      (T.pack filePath, T.pack rootPath)
  return packageName

-- | Try to read the specified file using utf-8 encoding first,
-- and latin1 otherwise
readFileUtf8Latin1 :: FilePath -> IO Text
readFileUtf8Latin1 filePath = catch @IOException (TIO.readFile filePath) $
  \e -> do
    putFmtLn
      "Unable to read {} with UTF-8 ({}), trying latin1 encoding..."
      (filePath, show e)
    decodeLatin1 <$> ByteString.readFile filePath

firstMiddleLast :: [a] -> Maybe (a, [a], a)
firstMiddleLast string = case string of
  x1 : x2 : xs -> Just (x1, init (x2 : xs), last (x2 : xs))
  _ -> Nothing

declToNormName :: String -> String
declToNormName declOpName = case firstMiddleLast declOpName of
  Just ('(', middle, ')') -> middle
  _ -> declOpName

infixToNormName :: String -> String
infixToNormName infixOpName = case firstMiddleLast infixOpName of
  Just ('`', middle, '`') -> middle
  _ -> infixOpName

onSymbolDecl :: Text -> State -> Text -> State
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

onFixityDecl :: Text -> State -> (Text, Text, Text) -> State
onFixityDecl packageName state@State {..} (rawFixDir, rawFixPrec, infixOpName) =
  let sPackageToOps' = case Map.lookup packageName' sPackageToOps of
        Nothing ->
          Map.insert
            packageName'
            (Map.singleton normOpName [fixDecl])
            sPackageToOps
        Just packageFixityMap ->
          case fromMaybe [] $ Map.lookup normOpName packageFixityMap of
            [] ->
              Map.insert
                packageName'
                (Map.insert normOpName [fixDecl] packageFixityMap)
                sPackageToOps
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

finalizePackageToOps ::
  Map String (Map String [FixityInfo]) ->
  (Map String FixityMap, [((String, String), [FixityInfo])])
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
      fmap (\(opName, fixs) -> ((packageName, opName), fixs)) opFixs

extractFixitiesFromFile ::
  -- | Hoogle database path
  FilePath ->
  State ->
  -- | Current file path
  FilePath ->
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
            (fromFixityDecl <$> fixityDecls fileContent)
        fromSymbolDecl match = capture @"declOpName" match
        fromFixityDecl match =
          ( capture @"fixDir" match,
            capture @"fixPrec" match,
            capture @"infixOpName" match
          )
        symbolDecls = [regex|(?m)^\s*?(?<declOpName>\([^)]+?\))\s*?::.*$|]
        fixityDecls = [regex|(?m)^\s*?(?<fixDir>infix[rl]?)\s+?(?<fixPrec>[0-9])\s+?(?<infixOpName>[^\s]+)\s*$|]
    return state'' {sProcessedFiles = sProcessedFiles + 1}

extractHoogleInfo :: FilePath -> IO (Map String FixityMap)
extractHoogleInfo hoogleDatabasePath = do
  hoogleFiles <- walkDir hoogleDatabasePath (const False)
  State {..} <-
    foldM
      (extractFixitiesFromFile hoogleDatabasePath)
      initialState
      hoogleFiles
  putFmtLn
    "{} hoogle files processed!"
    (Only sProcessedFiles)
  let (packageToOps, conflicts) = finalizePackageToOps sPackageToOps
  putFmtLn
    "Found {} operator declarations across {} packages for a total of \
    \{} distinct operators"
    (getCounts packageToOps)
  when (not (null conflicts)) $
    displayConflicts conflicts
  return packageToOps

displayConflicts :: [((String, String), [FixityInfo])] -> IO ()
displayConflicts conflicts = do
  putFmtLn
    "Found {} conflicting declarations within packages themselves:"
    (Only $ length conflicts)
  TIO.putStrLn $ T.intercalate "\n" conflictLines'
  where
    conflictLines' = concat $ conflictLines <$> sortedConflicts
    sortedConflicts =
      sortBy
        (\(packageOp1, _) (packageOp2, _) -> compare packageOp1 packageOp2)
        conflicts
    conflictLines ((packageName, opName), fixities) =
      format
        "(in {}) {}"
        (packageName, opName)
        : indentLines (showT <$> fixities)

limitMap :: Ord k => Int -> Map k v -> Map k v
limitMap n = Map.fromList . take n . Map.toList

getCounts :: Map String FixityMap -> (Int, Int, Int)
getCounts packageToOps = (declCount, packagesCount, distinctOpCount)
  where
    packagesCount = Map.size packageToOps
    declCount = sum $ Map.size <$> fixityMaps
    distinctOpCount =
      Set.size . Set.fromList . concat $
        Map.keys <$> fixityMaps
    fixityMaps = Map.elems packageToOps

extractHackageInfo :: FilePath -> IO (Map String Int)
extractHackageInfo filePath = do
  content <- TIO.readFile filePath
  let soup = filterBlankTags $ parseTags content
      tableBody =
        tail $
          takeWhile (not . tagCloseLit "tbody") $
            dropWhile (not . tagOpenLit "tbody" (const True)) soup
      processRow tags = case extractText <$> groupOn "td" tags of
        rawName : rawDlCount : _ -> return $ Just (name, dlCount)
          where
            name = T.unpack . T.strip . head $ T.split (== ' ') rawName
            dlCount = readT $ T.strip rawDlCount :: Int
        _ -> do
          putFmtLn
            "Invalid line: {}"
            (Only $ T.intercalate " " $ showT <$> tags)
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
  result <- Map.fromList <$> mapMaybeM processRow (groupOn "tr" tableBody)
  putFmtLn
    "Found popularity information for {} packages"
    (Only $ Map.size result)
  return result

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

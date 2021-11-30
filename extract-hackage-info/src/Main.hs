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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Format hiding (format)
import qualified Data.Text.Format as Format
import Data.Text.Format.Params (Params)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import GHC.Types.Fixity (FixityDirection (..))
import GHC.Utils.Monad (mapMaybeM)
import Options.Applicative
import Ormolu.Fixity
import System.Directory (listDirectory)
import System.FilePath (makeRelative, splitPath, (</>))
import System.Posix.Files (getFileStatus, isDirectory)
import Text.HTML.TagSoup (Tag (TagText), parseTags)
import Text.HTML.TagSoup.Match (tagCloseLit, tagOpenLit)
import Text.Regex.Pcre2 (capture, regex)

defaultOutputPath :: FilePath
defaultOutputPath = "extract-hackage-info/hackage-info.json"

baseInitialFixityMap :: HashMap String [FixityInfo]
baseInitialFixityMap =
  HashMap.singleton
    ":"
    [FixityInfo {fiDirection = Just InfixR, fiMinPrecedence = 5, fiMaxPrecedence = 5}]

unspecifiedFixityInfo :: FixityInfo
unspecifiedFixityInfo = FixityInfo (Just InfixL) 9 9

initialState :: State
initialState =
  State
    { sPackageToOps = HashMap.empty,
      sConflicts = HashMap.empty,
      sProcessedFiles = 0
    }

newtype SimpleException = SimpleException Text deriving (Eq, Show)

instance Exception SimpleException

data State = State
  { sPackageToOps :: HashMap String (HashMap String [FixityInfo]),
    sConflicts :: HashMap (String, String) [FixityInfo],
    sProcessedFiles :: Int
  }
  deriving (Eq)

data Config = Config
  { cfgHoogleDatabasePath :: FilePath,
    cfgHackageDatabasePath :: FilePath,
    cfgOutputPath :: FilePath,
    cfgDebugLimit :: Maybe Int
  }
  deriving (Eq, Show)

-- | format using the Strict variant of Data.Text
format :: forall ps. Params ps => Format -> ps -> Text
format f p = TL.toStrict $ Format.format f p

-- | Put a formatted string
putFmtLn :: forall ps. Params ps => Format -> ps -> IO ()
putFmtLn f p = TIO.putStrLn $ format f p

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
  paths <- forM (filter (not . exclude) ds) $ \d -> do
    let path = top </> d
    s <- getFileStatus path
    if isDirectory s
      then walkDir path exclude
      else return [path]
  return (concat paths)

getPackageName ::
  -- | Hoogle database path
  FilePath ->
  -- | Current file path
  FilePath ->
  IO Text
getPackageName hoogleDatabasePath filePath = do
  unless (hoogleDatabasePath `isPrefixOf` filePath) $
    throwIO . SimpleException $
      format
        "{} do not start with {}"
        (T.pack filePath, T.pack hoogleDatabasePath)
  let packageName =
        stripSuffix' "/" $
          T.pack . head . splitPath $
            makeRelative hoogleDatabasePath filePath
      stripSuffix' suffix txt = fromMaybe txt $ T.stripSuffix suffix txt
  when (T.null packageName) $
    throwIO . SimpleException $
      format
        "Extracted package name is empty for {} (base path = {})"
        (T.pack filePath, T.pack hoogleDatabasePath)
  return packageName

-- | Try to read the specified file using utf-8 encoding first, and latin1 otherwise
readFileUtf8Latin1 :: FilePath -> IO Text
readFileUtf8Latin1 filePath = catch @IOException (TIO.readFile filePath) $ \e -> do
  putFmtLn
    "Unable to read {} with UTF-8, trying latin1: {}"
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
  let sPackageToOps' = case HashMap.lookup packageName' sPackageToOps of
        Nothing
          | packageName' == "base" ->
              HashMap.insert
                packageName'
                (HashMap.insert normOpName [] baseInitialFixityMap)
                sPackageToOps
        Nothing ->
          HashMap.insert
            packageName'
            (HashMap.singleton normOpName [])
            sPackageToOps
        Just packageFixityMap -> case HashMap.lookup normOpName packageFixityMap of
          Nothing ->
            HashMap.insert
              packageName'
              (HashMap.insert normOpName [] packageFixityMap)
              sPackageToOps
          Just _ -> sPackageToOps
      normOpName = declToNormName . T.unpack $ declOpName
      packageName' = T.unpack packageName
   in state {sPackageToOps = sPackageToOps'}

onFixityDecl :: Text -> State -> (Text, Text, Text) -> State
onFixityDecl packageName state@State {..} (rawFixDir, rawFixPrec, infixOpName) =
  let (sPackageToOps', sConflicts') = case HashMap.lookup packageName' sPackageToOps of
        Nothing
          | packageName' == "base" ->
              ( HashMap.insert
                  packageName'
                  (HashMap.insert normOpName [fixDecl] baseInitialFixityMap)
                  sPackageToOps,
                sConflicts
              )
        Nothing ->
          ( HashMap.insert
              packageName'
              (HashMap.singleton normOpName [fixDecl])
              sPackageToOps,
            sConflicts
          )
        Just packageFixityMap -> case fromMaybe [] $ HashMap.lookup normOpName packageFixityMap of
          [] ->
            ( HashMap.insert
                packageName'
                (HashMap.insert normOpName [fixDecl] packageFixityMap)
                sPackageToOps,
              sConflicts
            )
          fixDecls
            | fixDecl `elem` fixDecls ->
                ( sPackageToOps,
                  sConflicts
                )
          fixDecls ->
            ( HashMap.insert
                packageName'
                (HashMap.insert normOpName (fixDecl : fixDecls) packageFixityMap)
                sPackageToOps,
              HashMap.insert
                (packageName', normOpName)
                (fixDecl : fixDecls)
                sConflicts
            )
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
   in state {sPackageToOps = sPackageToOps', sConflicts = sConflicts'}

finalizePackageToOps ::
  HashMap String (HashMap String [FixityInfo]) ->
  HashMap String FixityMap
finalizePackageToOps = HashMap.map (HashMap.map finalize)
  where
    finalize = \case
      [] -> unspecifiedFixityInfo
      fs -> sconcat . NE.fromList $ fs

extractFixitiesFromFile ::
  -- | Hoogle database path
  FilePath ->
  State ->
  -- | Current file path
  FilePath ->
  IO State
extractFixitiesFromFile hoogleDatabasePath state@State {sProcessedFiles} filePath = do
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

extractHoogleInfo :: FilePath -> IO (HashMap String FixityMap)
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
  let packageToOps = finalizePackageToOps sPackageToOps
  putFmtLn
    "Found {} operator declarations across {} packages for a total of {} distinct operators"
    (getCounts packageToOps)
  when (HashMap.size sConflicts > 0) $
    displayConflicts sConflicts
  return packageToOps

displayConflicts :: HashMap (String, String) [FixityInfo] -> IO ()
displayConflicts hashmap = do
  putFmtLn
    "Found {} conflicting declarations within packages themselves:"
    (Only $ HashMap.size hashmap)
  TIO.putStrLn $ T.intercalate "\n" conflictLines'
  where
    conflictLines' = concat $ conflictLines <$> sortedConflicts
    sortedConflicts =
      sortBy
        (\(packageOp1, _) (packageOp2, _) -> compare packageOp1 packageOp2)
        (HashMap.toList hashmap)
    conflictLines ((packageName, opName), fixities) =
      format
        "{} in {}:"
        (packageName, opName)
        : indentLines (renderFixityInfo <$> fixities)

renderFixityInfo :: FixityInfo -> Text
renderFixityInfo (FixityInfo dir pmin pmax) =
  format "FixityInfo {} {} {}" (strDir, pmin, pmax)
  where
    strDir =
      case dir of
        Nothing -> "Nothing" :: Text
        Just InfixN -> "(Just InfixN)"
        Just InfixR -> "(Just InfixR)"
        Just InfixL -> "(Just InfixL)"

limitMapWith ::
  (Eq k, Hashable k) =>
  (v -> v') ->
  Int ->
  HashMap k v ->
  HashMap k v'
limitMapWith f n hashmap =
  HashMap.fromList $
    (\k -> (k, f . fromJust $ HashMap.lookup k hashmap)) <$> limitedKeys
  where
    limitedKeys = take n $ HashMap.keys hashmap

getCounts :: HashMap String FixityMap -> (Int, Int, Int)
getCounts packageToOps = (declCount, packagesCount, distinctOpCount)
  where
    packagesCount = HashMap.size packageToOps
    declCount = sum $ HashMap.size <$> fixityMaps
    distinctOpCount =
      HashSet.size . HashSet.fromList . concat $
        HashMap.keys <$> fixityMaps
    fixityMaps = HashMap.elems packageToOps

extractHackageInfo :: FilePath -> IO (HashMap String Int)
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
  result <- HashMap.fromList <$> mapMaybeM processRow (groupOn "tr" tableBody)
  putFmtLn
    "Found popularity information for {} packages"
    (Only $ HashMap.size result)
  return result

configParserInfo :: ParserInfo Config
configParserInfo = info (helper <*> configParser) fullDesc
  where
    configParser :: Parser Config
    configParser =
      Config
        <$> (strArgument . mconcat)
          [ metavar "HOOGLE_DATABASE_PATH",
            help "Download: mkdir -p hoogle-database && curl https://hackage.haskell.org/packages/hoogle.tar.gz | tar -xz -C hoogle-database"
          ]
        <*> (strArgument . mconcat)
          [ metavar "HACKAGE_DATABASE_PATH",
            help "Donwload: curl https://hackage.haskell.org/packages/browse -o hackage-database.html"
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

-- | Entry point of the program.
main :: IO ()
main = do
  Config {..} <- execParser configParserInfo
  packageToOps <- extractHoogleInfo cfgHoogleDatabasePath
  packageToPop <- extractHackageInfo cfgHackageDatabasePath
  let (packageToOps', packageToPop') = case cfgDebugLimit of
        Nothing -> (packageToOps, packageToPop)
        Just n ->
          ( limitMapWith (limitMapWith id n) n packageToOps,
            limitMapWith id n packageToPop
          )
  encodeFile cfgOutputPath $
    HackageInfo packageToOps' packageToPop'

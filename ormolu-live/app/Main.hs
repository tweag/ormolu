{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception qualified as E
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Data.Knob qualified as Knob
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign
import Foreign.C.Types
import GHC.Driver.Ppr (showSDocUnsafe)
import GHC.Generics (Generic)
import GHC.Hs.Dump qualified as Dump
import Ormolu
import Ormolu.Config qualified as O
import Ormolu.Exception qualified as O
import Ormolu.Fixity.Internal qualified as O
import Ormolu.Parser qualified as O
import Ormolu.Parser.Result as O
import Ormolu.Terminal qualified as O
import System.Environment (setEnv)
import System.IO (IOMode (..))

main :: IO ()
main = mempty

-- marshalling

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
formatRaw inputPtr inputLen outputPtrPtr = do
  Just input <-
    A.decodeStrict' <$> BU.unsafePackMallocCStringLen (inputPtr, inputLen)
  outputBytes <- BL.toStrict . A.encode <$> format input
  BU.unsafeUseAsCStringLen outputBytes \(buf, len) -> do
    outputPtr <- mallocBytes len
    poke outputPtrPtr outputPtr
    copyBytes outputPtr buf len
    pure len

foreign export ccall initFixityDB :: Ptr CChar -> Int -> IO ()

initFixityDB :: Ptr CChar -> Int -> IO ()
initFixityDB ptr len = do
  let IntPtr ptr' = ptrToIntPtr ptr
  setEnv "ORMOLU_HACKAGE_INFO" $ show (ptr', len)

-- actual logic

data Input = Input
  { inputStr :: Text,
    checkIdempotence :: Bool,
    unsafeMode :: Bool,
    formatBackpack :: Bool,
    showAST :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.FromJSON)

data Output = Output
  { fmtStr :: Text,
    inputAST :: Text,
    outputAST :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.ToJSON)

format :: Input -> IO Output
format Input {..} = do
  output <-
    (Right <$> ormolu cfg "<input>" inputStr) `E.catch` \ex -> do
      knob <- Knob.newKnob mempty
      Knob.withFileHandle knob "err" WriteMode $
        O.runTerm (O.printOrmoluException ex) Never
      Left . T.decodeUtf8 <$> Knob.getContents knob
  inputAST <- if showAST then prettyAST cfg inputStr else pure T.empty
  outputAST <- case output of
    Right src' | showAST -> prettyAST cfg src'
    _ -> pure T.empty
  pure Output {fmtStr = either id id output, ..}
  where
    cfg =
      defaultConfig
        { cfgCheckIdempotence = checkIdempotence,
          cfgUnsafe = unsafeMode,
          cfgSourceType = if formatBackpack then SignatureSource else ModuleSource
        }

prettyAST :: Config RegionIndices -> Text -> IO Text
prettyAST cfg src = do
  (_, eSnippets) <-
    O.parseModule cfgWithDeltas (O.LazyFixityMap []) "<input>" src
  pure case eSnippets of
    Left e -> T.pack $ show e
    Right snippets -> T.unlines $ showSnippet <$> snippets
  where
    cfgWithDeltas = O.regionIndicesToDeltas (length (T.lines src)) <$> cfg
    showSnippet = \case
      O.ParsedSnippet O.ParseResult {..} ->
        T.pack
          . showSDocUnsafe
          . Dump.showAstData Dump.NoBlankSrcSpan Dump.NoBlankEpAnnotations
          $ prParsedSource
      O.RawSnippet r -> r

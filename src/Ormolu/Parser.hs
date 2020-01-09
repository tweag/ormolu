{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Parser for Haskell source code.
module Ormolu.Parser
  ( parseModule,
    manualExts,
  )
where

import qualified CmdLineParser as GHC
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List ((\\), foldl', isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import DynFlags as GHC
import qualified FastString as GHC
import GHC hiding (IE, UnicodeSyntax)
import GHC.DynFlags (baseDynFlags)
import GHC.LanguageExtensions.Type (Extension (..))
import qualified HeaderInfo as GHC
import qualified HscTypes as GHC
import qualified Lexer as GHC
import Ormolu.Config
import Ormolu.Exception
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import qualified Outputable as GHC
import qualified Panic as GHC
import qualified Parser as GHC
import qualified StringBuffer as GHC

-- | Parse a complete module from string.
parseModule ::
  MonadIO m =>
  -- | Ormolu configuration
  Config ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  String ->
  m
    ( [GHC.Warn],
      Either (SrcSpan, String) ParseResult
    )
parseModule Config {..} path input' = liftIO $ do
  let (input, extraComments) = extractCommentsFromLines path input'
  -- It's important that 'setDefaultExts' is done before
  -- 'parsePragmasIntoDynFlags', because otherwise we might enable an
  -- extension that was explicitly disabled in the file.
  let baseFlags =
        GHC.setGeneralFlag'
          GHC.Opt_Haddock
          (setDefaultExts baseDynFlags)
      extraOpts = dynOptionToLocatedStr <$> cfgDynOptions
  (warnings, dynFlags) <-
    parsePragmasIntoDynFlags baseFlags extraOpts path input' >>= \case
      Right res -> pure res
      Left err ->
        let loc =
              mkSrcSpan
                (mkSrcLoc (GHC.mkFastString path) 1 1)
                (mkSrcLoc (GHC.mkFastString path) 1 1)
         in throwIO (OrmoluParsingFailed loc err)
  when (GHC.xopt Cpp dynFlags && not cfgTolerateCpp) $
    throwIO (OrmoluCppEnabled path)
  let useRecordDot =
        "record-dot-preprocessor" == pgm_F dynFlags
          || any
            (("RecordDotPreprocessor" ==) . moduleNameString)
            (pluginModNames dynFlags)
      r = case runParser GHC.parseModule dynFlags path input of
        GHC.PFailed _ ss m ->
          Left (ss, GHC.showSDoc dynFlags m)
        GHC.POk pstate pmod ->
          let (comments, exts, shebangs) = mkCommentStream extraComments pstate
           in Right
                ParseResult
                  { prParsedSource = pmod,
                    prAnns = mkAnns pstate,
                    prCommentStream = comments,
                    prExtensions = exts,
                    prShebangs = shebangs,
                    prUseRecordDot = useRecordDot
                  }
  return (warnings, r)

-- | Extensions that are not enabled automatically and should be activated
-- by user.
manualExts :: [Extension]
manualExts =
  [ Arrows, -- steals proc
    Cpp, -- forbidden
    BangPatterns, -- makes certain patterns with ! fail
    PatternSynonyms, -- steals the pattern keyword
    RecursiveDo, -- steals the rec keyword
    StaticPointers, -- steals static keyword
    TransformListComp, -- steals the group keyword
    UnboxedTuples, -- breaks (#) lens operator
    MagicHash, -- screws {-# these things #-}
    TypeApplications, -- steals (@) operator on some cases
    AlternativeLayoutRule,
    AlternativeLayoutRuleTransitional,
    MonadComprehensions,
    UnboxedSums,
    UnicodeSyntax, -- gives special meanings to operators like (→)
    TemplateHaskellQuotes -- enables TH subset of quasi-quotes, this
    -- apparently interferes with QuasiQuotes in
    -- weird ways
  ]

----------------------------------------------------------------------------
-- Helpers (taken from ghc-exactprint)

-- | Run a 'GHC.P' computation.
runParser ::
  -- | Computation to run
  GHC.P a ->
  -- | Dynamic flags
  GHC.DynFlags ->
  -- | Module path
  FilePath ->
  -- | Module contents
  String ->
  -- | Parse result
  GHC.ParseResult a
runParser parser flags filename input = GHC.unP parser parseState
  where
    location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
    buffer = GHC.stringToStringBuffer input
    parseState = GHC.mkPState flags buffer location

-- | Transform given lines possibly returning comments extracted from them.
-- This handles LINE pragmas and shebangs.
extractCommentsFromLines ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | List of lines from that file
  String ->
  -- | Adjusted lines together with comments extracted from them
  (String, [Located String])
extractCommentsFromLines path =
  unlines' . unzip . zipWith (extractCommentFromLine path) [1 ..] . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

-- | Transform a given line possibly returning a comment extracted from it.
extractCommentFromLine ::
  -- | File name, just to use in the spans
  FilePath ->
  -- | Line number of this line
  Int ->
  -- | The actual line
  String ->
  -- | Adjusted line and possibly a comment extracted from it
  (String, Maybe (Located String))
extractCommentFromLine path line s
  | "{-# LINE" `isPrefixOf` s =
    let (pragma, res) = getPragma s
        size = length pragma
        ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (size + 1))
     in (res, Just $ L ss pragma)
  | isShebang s =
    let ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (length s))
     in ("", Just $ L ss s)
  | otherwise = (s, Nothing)
  where
    mkSrcLoc' = mkSrcLoc (GHC.mkFastString path) line

-- | Take a line pragma and output its replacement (where line pragma is
-- replaced with spaces) and the contents of the pragma itself.
getPragma ::
  -- | Pragma line to analyze
  String ->
  -- | Contents of the pragma and its replacement line
  (String, String)
getPragma [] = error "Ormolu.Parser.getPragma: input must not be empty"
getPragma s@(x : xs)
  | "#-}" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
    let (prag, remline) = getPragma xs
     in (x : prag, ' ' : remline)

-- | Enable all language extensions that we think should be enabled by
-- default for ease of use.
setDefaultExts :: DynFlags -> DynFlags
setDefaultExts flags = foldl' GHC.xopt_set flags autoExts
  where
    autoExts = allExts \\ manualExts
    allExts = [minBound .. maxBound]

----------------------------------------------------------------------------
-- More helpers (taken from HLint)

parsePragmasIntoDynFlags ::
  -- | Pre-set 'DynFlags'
  DynFlags ->
  -- | Extra options (provided by user)
  [Located String] ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  String ->
  IO (Either String ([GHC.Warn], DynFlags))
parsePragmasIntoDynFlags flags extraOpts filepath str =
  catchErrors $ do
    let opts = GHC.getOptions flags (GHC.stringToStringBuffer str) filepath
    (flags', leftovers, warnings) <-
      parseDynamicFilePragma flags (opts <> extraOpts)
    case NE.nonEmpty leftovers of
      Nothing -> return ()
      Just unrecognizedOpts ->
        throwIO (OrmoluUnrecognizedOpts (unLoc <$> unrecognizedOpts))
    let flags'' = flags' `gopt_set` Opt_KeepRawTokenStream
    return $ Right (warnings, flags'')
  where
    catchErrors act =
      GHC.handleGhcException
        reportErr
        (GHC.handleSourceError reportErr act)
    reportErr e = return $ Left (show e)

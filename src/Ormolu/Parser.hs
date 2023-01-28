{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Parser for Haskell source code.
module Ormolu.Parser
  ( parseModule,
    manualExts,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Functor
import Data.Generics
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC.Data.Bag (bagToList)
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as GHC
import qualified GHC.Driver.CmdLine as GHC
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session as GHC
import GHC.DynFlags (baseDynFlags)
import GHC.Hs hiding (UnicodeSyntax)
import GHC.LanguageExtensions.Type (Extension (..))
import qualified GHC.Parser as GHC
import qualified GHC.Parser.Header as GHC
import qualified GHC.Parser.Lexer as GHC
import GHC.Types.Error (NoDiagnosticOpts (..), getMessages)
import qualified GHC.Types.SourceError as GHC (handleSourceError)
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Outputable (defaultSDocContext)
import qualified GHC.Utils.Panic as GHC
import Ormolu.Config
import Ormolu.Exception
import Ormolu.Fixity (LazyFixityMap)
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Processing.Common
import Ormolu.Processing.Preprocess
import Ormolu.Utils (incSpanLine, showOutputable, textToStringBuffer)

-- | Parse a complete module from string.
parseModule ::
  (MonadIO m) =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | Fixity map to include in the resulting 'ParseResult's
  LazyFixityMap ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  Text ->
  m
    ( [GHC.Warn],
      Either (SrcSpan, String) [SourceSnippet]
    )
parseModule config@Config {..} fixityMap path rawInput = liftIO $ do
  -- It's important that 'setDefaultExts' is done before
  -- 'parsePragmasIntoDynFlags', because otherwise we might enable an
  -- extension that was explicitly disabled in the file.
  let baseFlags =
        GHC.setGeneralFlag'
          GHC.Opt_Haddock
          (setDefaultExts baseDynFlags)
      extraOpts = dynOptionToLocatedStr <$> cfgDynOptions
  (warnings, dynFlags) <-
    parsePragmasIntoDynFlags baseFlags extraOpts path rawInput >>= \case
      Right res -> pure res
      Left err ->
        let loc =
              mkSrcSpan
                (mkSrcLoc (GHC.mkFastString path) 1 1)
                (mkSrcLoc (GHC.mkFastString path) 1 1)
         in throwIO (OrmoluParsingFailed loc err)
  let cppEnabled = EnumSet.member Cpp (GHC.extensionFlags dynFlags)
  snippets <- runExceptT . forM (preprocess cppEnabled cfgRegion rawInput) $ \case
    Right region ->
      fmap ParsedSnippet . ExceptT $
        parseModuleSnippet (config $> region) fixityMap dynFlags path rawInput
    Left raw -> pure $ RawSnippet raw
  pure (warnings, snippets)

parseModuleSnippet ::
  (MonadIO m) =>
  Config RegionDeltas ->
  LazyFixityMap ->
  DynFlags ->
  FilePath ->
  Text ->
  m (Either (SrcSpan, String) ParseResult)
parseModuleSnippet Config {..} fixityMap dynFlags path rawInput = liftIO $ do
  let (input, indent) = removeIndentation . linesInRegion cfgRegion $ rawInput
  let pStateErrors pstate =
        let errs = bagToList . getMessages $ GHC.getPsErrorMessages pstate
            fixupErrSpan = incSpanLine (regionPrefixLength cfgRegion)
            rateSeverity = \case
              SevError -> 1 :: Int
              SevWarning -> 2
              SevIgnore -> 3
            showErr (errMsgDiagnostic -> err) = codeMsg <> msg
              where
                codeMsg = case diagnosticCode err of
                  Just code -> "[" <> showOutputable code <> "] "
                  Nothing -> ""
                msg =
                  showOutputable
                    . formatBulleted defaultSDocContext
                    . diagnosticMessage NoDiagnosticOpts
                    $ err
         in case L.sortOn (rateSeverity . errMsgSeverity) errs of
              [] -> Nothing
              err : _ ->
                Just (fixupErrSpan (errMsgSpan err), showErr err)
      parser = case cfgSourceType of
        ModuleSource -> GHC.parseModule
        SignatureSource -> GHC.parseSignature
      r = case runParser parser dynFlags path input of
        GHC.PFailed pstate ->
          case pStateErrors pstate of
            Just err -> Left err
            Nothing -> error "PFailed does not have an error"
        GHC.POk pstate (L _ (normalizeModule -> hsModule)) ->
          case pStateErrors pstate of
            -- Some parse errors (pattern/arrow syntax in expr context)
            -- do not cause a parse error, but they are replaced with "_"
            -- by the parser and the modified AST is propagated to the
            -- later stages; but we fail in those cases.
            Just err -> Left err
            Nothing ->
              let (stackHeader, pragmas, comments) =
                    mkCommentStream input hsModule
               in Right
                    ParseResult
                      { prParsedSource = hsModule,
                        prSourceType = cfgSourceType,
                        prStackHeader = stackHeader,
                        prPragmas = pragmas,
                        prCommentStream = comments,
                        prExtensions = GHC.extensionFlags dynFlags,
                        prFixityOverrides = cfgFixityOverrides,
                        prFixityMap = fixityMap,
                        prIndent = indent
                      }
  return r

-- | Normalize a 'HsModule' by sorting its import\/export lists, dropping
-- blank comments, etc.
normalizeModule :: HsModule GhcPs -> HsModule GhcPs
normalizeModule hsmod =
  everywhere
    (extT (mkT dropBlankTypeHaddocks) patchContext)
    hsmod
      { hsmodImports =
          normalizeImports (hsmodImports hsmod),
        hsmodDecls =
          filter (not . isBlankDocD . unLoc) (hsmodDecls hsmod),
        hsmodExt =
          (hsmodExt hsmod)
            { hsmodHaddockModHeader =
                mfilter (not . isBlankDocString) (hsmodHaddockModHeader (hsmodExt hsmod))
            },
        hsmodExports =
          (fmap . fmap) (filter (not . isBlankDocIE . unLoc)) (hsmodExports hsmod)
      }
  where
    isBlankDocString = all isSpace . renderHsDocString . hsDocString . unLoc
    isBlankDocD = \case
      DocD _ s -> isBlankDocString $ docDeclDoc s
      _ -> False
    isBlankDocIE = \case
      IEGroup _ _ s -> isBlankDocString s
      IEDoc _ s -> isBlankDocString s
      _ -> False
    dropBlankTypeHaddocks = \case
      L _ (HsDocTy _ ty s) :: LHsType GhcPs
        | isBlankDocString s -> ty
      a -> a
    patchContext :: LHsContext GhcPs -> LHsContext GhcPs
    patchContext = fmap $ \case
      [x@(L _ (HsParTy _ _))] -> [x]
      [x@(L lx _)] -> [L lx (HsParTy EpAnnNotUsed x)]
      xs -> xs

-- | Enable all language extensions that we think should be enabled by
-- default for ease of use.
setDefaultExts :: DynFlags -> DynFlags
setDefaultExts flags = L.foldl' xopt_set (lang_set flags (Just Haskell2010)) autoExts
  where
    autoExts = allExts L.\\ manualExts
    allExts = [minBound .. maxBound]

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
    AlternativeLayoutRule,
    AlternativeLayoutRuleTransitional,
    MonadComprehensions,
    UnboxedSums,
    UnicodeSyntax, -- gives special meanings to operators like (→)
    TemplateHaskell, -- changes how $foo is parsed
    TemplateHaskellQuotes, -- enables TH subset of quasi-quotes, this
    -- apparently interferes with QuasiQuotes in
    -- weird ways
    ImportQualifiedPost, -- affects how Ormolu renders imports, so the
    -- decision of enabling this style is left to the user
    NegativeLiterals, -- with this, `- 1` and `-1` have differing AST
    LexicalNegation, -- implies NegativeLiterals
    LinearTypes, -- steals the (%) type operator in some cases
    OverloadedRecordDot, -- f.g parses differently
    OverloadedRecordUpdate, -- qualified fields are not supported
    OverloadedLabels -- a#b is parsed differently
  ]

-- | Run a 'GHC.P' computation.
runParser ::
  -- | Computation to run
  GHC.P a ->
  -- | Dynamic flags
  GHC.DynFlags ->
  -- | Module path
  FilePath ->
  -- | Module contents
  Text ->
  -- | Parse result
  GHC.ParseResult a
runParser parser flags filename input = GHC.unP parser parseState
  where
    location = mkRealSrcLoc (GHC.mkFastString filename) 1 1
    buffer = textToStringBuffer input
    parseState = GHC.initParserState (initParserOpts flags) buffer location

----------------------------------------------------------------------------
-- Helpers taken from HLint

parsePragmasIntoDynFlags ::
  -- | Pre-set 'DynFlags'
  DynFlags ->
  -- | Extra options (provided by user)
  [Located String] ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  Text ->
  IO (Either String ([GHC.Warn], DynFlags))
parsePragmasIntoDynFlags flags extraOpts filepath str =
  catchErrors $ do
    let (_warnings, fileOpts) =
          GHC.getOptions
            (initParserOpts flags)
            (textToStringBuffer str)
            filepath
    (flags', leftovers, warnings) <-
      parseDynamicFilePragma flags (extraOpts <> fileOpts)
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

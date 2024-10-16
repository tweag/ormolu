{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Generics hiding (orElse)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import GHC.Builtin.Names (mAIN_NAME)
import GHC.Data.Bag (bagToList)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString qualified as GHC
import GHC.Data.Maybe (orElse)
import GHC.Data.StringBuffer (StringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Errors.Types qualified as GHC
import GHC.Driver.Session as GHC
import GHC.DynFlags (baseDynFlags)
import GHC.Hs hiding (UnicodeSyntax)
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Parser qualified as GHC
import GHC.Parser.Annotation qualified as GHC
import GHC.Parser.Header qualified as GHC
import GHC.Parser.Lexer qualified as GHC
import GHC.Types.Error qualified as GHC
import GHC.Types.SourceError qualified as GHC
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Exception (ExceptionMonad)
import GHC.Utils.Panic qualified as GHC
import Ormolu.Config
import Ormolu.Exception
import Ormolu.Fixity hiding (packageFixityMap)
import Ormolu.Fixity.Imports (applyModuleReexports, extractFixityImports)
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Processing.Common
import Ormolu.Processing.Cpp (eraseCppLines)
import Ormolu.Processing.Preprocess
import Ormolu.Utils (incSpanLine, showOutputable, textToStringBuffer)

-- | Parse a complete module from 'Text'.
parseModule ::
  (MonadIO m) =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | Package fixity map
  PackageFixityMap ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  Text ->
  m
    ( GHC.DriverMessages,
      Either (SrcSpan, String) [SourceSnippet]
    )
parseModule config@Config {..} packageFixityMap path rawInput = liftIO $ do
  -- It's important that 'setDefaultExts' is done before
  -- 'parsePragmasIntoDynFlags', because otherwise we might enable an
  -- extension that was explicitly disabled in the file.
  let baseFlags =
        GHC.setGeneralFlag'
          GHC.Opt_Haddock
          (setDefaultExts baseDynFlags)
      extraOpts = dynOptionToLocatedStr <$> cfgDynOptions
      rawInputStringBuffer = textToStringBuffer (eraseCppLines rawInput)
      beginningLoc =
        mkSrcSpan
          (mkSrcLoc (GHC.mkFastString path) 1 1)
          (mkSrcLoc (GHC.mkFastString path) 1 1)
  (warnings, dynFlags) <-
    parsePragmasIntoDynFlags baseFlags extraOpts path rawInputStringBuffer >>= \case
      Right res -> pure res
      Left err -> throwIO (OrmoluParsingFailed beginningLoc err)
  let cppEnabled = EnumSet.member Cpp (GHC.extensionFlags dynFlags)
      implicitPrelude = EnumSet.member ImplicitPrelude (GHC.extensionFlags dynFlags)
  fixityImports <-
    parseImports dynFlags implicitPrelude path rawInputStringBuffer >>= \case
      Right res ->
        pure (applyModuleReexports cfgModuleReexports (extractFixityImports res))
      Left err ->
        throwIO (OrmoluParsingFailed beginningLoc err)
  let modFixityMap =
        applyFixityOverrides
          cfgFixityOverrides
          (moduleFixityMap packageFixityMap fixityImports)
  snippets <- runExceptT . forM (preprocess cppEnabled cfgRegion rawInput) $ \case
    Right region ->
      fmap ParsedSnippet . ExceptT $
        parseModuleSnippet (config $> region) modFixityMap dynFlags path rawInput
    Left raw -> pure $ RawSnippet raw
  pure (warnings, snippets)

parseModuleSnippet ::
  (MonadIO m) =>
  Config RegionDeltas ->
  ModuleFixityMap ->
  DynFlags ->
  FilePath ->
  Text ->
  m (Either (SrcSpan, String) ParseResult)
parseModuleSnippet Config {..} modFixityMap dynFlags path rawInput = liftIO $ do
  let (input, indent) = removeIndentation . linesInRegion cfgRegion $ rawInput
  let pStateErrors pstate =
        let errs = bagToList . GHC.getMessages $ GHC.getPsErrorMessages pstate
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
                    . formatBulleted
                    . diagnosticMessage GHC.NoDiagnosticOpts
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
                        prModuleFixityMap = modFixityMap,
                        prIndent = indent
                      }
  return r

-- | Normalize a 'HsModule' by sorting its import\/export lists, dropping
-- blank comments, etc.
normalizeModule :: HsModule GhcPs -> HsModule GhcPs
normalizeModule hsmod =
  everywhere
    ( mkT dropBlankTypeHaddocks
        `extT` dropBlankDataDeclHaddocks
        `extT` patchContext
        `extT` patchExprContext
    )
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
    dropBlankDataDeclHaddocks = \case
      ConDeclGADT {con_doc = Just s, ..} :: ConDecl GhcPs
        | isBlankDocString s -> ConDeclGADT {con_doc = Nothing, ..}
      ConDeclH98 {con_doc = Just s, ..} :: ConDecl GhcPs
        | isBlankDocString s -> ConDeclH98 {con_doc = Nothing, ..}
      a -> a

    -- For constraint contexts (both in types and in expressions), normalize
    -- parenthesis as decided in https://github.com/tweag/ormolu/issues/264.
    patchContext :: LHsContext GhcPs -> LHsContext GhcPs
    patchContext = fmap $ \case
      [x@(L _ (HsParTy _ _))] -> [x]
      [x@(L lx _)] -> [L lx (HsParTy noAnn x)]
      xs -> xs
    patchExprContext :: LHsExpr GhcPs -> LHsExpr GhcPs
    patchExprContext = fmap $ \case
      x@(HsQual _ (L _ [L _ HsPar {}]) _) -> x
      HsQual l0 (L l1 [x@(L lx _)]) e -> HsQual l0 (L l1 [L lx (HsPar noAnn x)]) e
      x -> x

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
    OverloadedLabels, -- a#b is parsed differently
    ExtendedLiterals, -- 1#Word32 is parsed differently
    MultilineStrings -- """""" is parsed differently
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

-- | Detect pragmas in the given input and return them as a collection of
-- 'DynFlags'.
parsePragmasIntoDynFlags ::
  -- | Pre-set 'DynFlags'
  DynFlags ->
  -- | Extra options (provided by user)
  [Located String] ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  StringBuffer ->
  IO (Either String (GHC.DriverMessages, DynFlags))
parsePragmasIntoDynFlags flags extraOpts filepath input =
  catchGhcErrors $ do
    let (_warnings, fileOpts) =
          GHC.getOptions
            (initParserOpts flags)
            input
            filepath
    (flags', leftovers, warnings) <-
      parseDynamicFilePragma flags (extraOpts <> fileOpts)
    case NE.nonEmpty leftovers of
      Nothing -> return ()
      Just unrecognizedOpts ->
        throwIO (OrmoluUnrecognizedOpts (unLoc <$> unrecognizedOpts))
    let flags'' = flags' `gopt_set` Opt_KeepRawTokenStream
    return $ Right (warnings, flags'')

-- | Detect the collection of imports used in the given input.
parseImports ::
  -- | Pre-set 'DynFlags'
  DynFlags ->
  -- | Implicit Prelude?
  Bool ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for the parser
  StringBuffer ->
  IO (Either String [LImportDecl GhcPs])
parseImports flags implicitPrelude filepath input =
  case GHC.unP GHC.parseHeader (GHC.initParserState popts input loc) of
    GHC.PFailed pst ->
      return $ Left (showOutputable (GHC.getPsErrorMessages pst))
    GHC.POk pst rdr_module ->
      return $
        let (_warnings, errors) = GHC.getPsMessages pst
         in if not (isEmptyMessages errors)
              then Left (showOutputable (GHC.GhcPsMessage <$> errors))
              else
                let hsmod = unLoc rdr_module
                    mmoduleName = hsmodName hsmod
                    main_loc = srcLocSpan (mkSrcLoc (GHC.mkFastString filepath) 1 1)
                    mod' = mmoduleName `orElse` L (GHC.noAnnSrcSpan main_loc) mAIN_NAME
                    explicitImports = hsmodImports hsmod
                    implicitImports =
                      GHC.mkPrelImports (unLoc mod') main_loc implicitPrelude explicitImports
                 in Right (explicitImports ++ implicitImports)
  where
    popts = initParserOpts flags
    loc = mkRealSrcLoc (GHC.mkFastString filepath) 1 1

-- | Catch and report GHC errors.
catchGhcErrors :: (ExceptionMonad m) => m (Either String a) -> m (Either String a)
catchGhcErrors m =
  GHC.handleGhcException
    reportErr
    (GHC.handleSourceError reportErr m)
  where
    reportErr e = return $ Left (show e)

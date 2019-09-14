{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Parser for Haskell source code.

module Ormolu.Parser
  ( parseModule
  , manualExts
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isPrefixOf, foldl', (\\))
import Data.Maybe (catMaybes)
import GHC hiding (IE, parseModule, parser)
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Paths (libdir)
import Ormolu.Config
import Ormolu.Exception
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import qualified CmdLineParser as GHC
import qualified DynFlags as GHC
import qualified FastString as GHC
import qualified HeaderInfo as GHC
import qualified Lexer as GHC
import qualified Outputable as GHC
import qualified Parser as GHC
import qualified SrcLoc as GHC
import qualified StringBuffer as GHC

-- | Parse a complete module from string.

parseModule
  :: MonadIO m
  => Config             -- ^ Ormolu configuration
  -> FilePath           -- ^ File name (only for source location annotations)
  -> String             -- ^ Input for parser
  -> m ( [GHC.Warn]
       , Either (SrcSpan, String) ParseResult
       )
parseModule Config {..} path input' = liftIO $ do
  let (input, extraComments) = stripLinePragmas path input'
  (ws, dynFlags) <- ghcWrapper $ do
    dynFlags0 <- initDynFlagsPure path input
    (dynFlags1, _, ws) <- GHC.parseDynamicFilePragma
      dynFlags0
      (dynOptionToLocatedStr <$> cfgDynOptions)
    return (ws, dynFlags1)
  -- NOTE It's better to throw this outside of 'ghcWrapper' because
  -- otherwise the exception will be wrapped as a GHC panic, which we don't
  -- want.
  when (GHC.xopt Cpp dynFlags && not cfgTolerateCpp) $
   throwIO (OrmoluCppEnabled path)
  let r = case runParser GHC.parseModule dynFlags path input of
        GHC.PFailed _ ss m ->
          Left (ss, GHC.showSDoc dynFlags m)
        GHC.POk pstate pmod ->
          let (comments, exts) = mkCommentStream extraComments pstate
          in Right ParseResult
            { prParsedSource = pmod
            , prAnns = mkAnns pstate
            , prCommentStream = comments
            , prExtensions = exts
            }
  return (ws, r)

-- | Extensions that are not enabled automatically and should be activated
-- by user.

manualExts :: [Extension]
manualExts =
  [ Arrows -- steals proc
  , Cpp -- forbidden
  , BangPatterns -- makes certain patterns with ! fail
  , PatternSynonyms -- steals the pattern keyword
  , RecursiveDo -- steals the rec keyword
  , StaticPointers -- steals static keyword
  , TransformListComp -- steals the group keyword
  , UnboxedTuples -- breaks (#) lens operator
  , MagicHash -- screws {-# these things #-}
  , TypeApplications -- steals (@) operator on some cases
  , AlternativeLayoutRule
  , AlternativeLayoutRuleTransitional
  , MonadComprehensions
  , UnboxedSums
  , UnicodeSyntax -- gives special meanings to operators like (→)
  , TemplateHaskellQuotes -- enables TH subset of quasi-quotes, this
                          -- apparently interferes with QuasiQuotes in
                          -- weird ways
  ]

----------------------------------------------------------------------------
-- Helpers (taken from ghc-exactprint)

-- | Requires GhcMonad constraint because there is no pure variant of
-- 'parseDynamicFilePragma'. Yet, in constrast to 'initDynFlags', it does
-- not (try to) read the file at filepath, but solely depends on the module
-- source in the input string.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of package
-- environment files. However this only works if there is no invocation of
-- 'setSessionDynFlags' before calling 'initDynFlagsPure'. See GHC tickets
-- #15513, #15541.

initDynFlagsPure
  :: GHC.GhcMonad m
  => FilePath                   -- ^ Module path
  -> String                     -- ^ Module contents
  -> m GHC.DynFlags             -- ^ Dynamic flags for that module
initDynFlagsPure fp input = do
  -- I was told we could get away with using the 'unsafeGlobalDynFlags'. as
  -- long as 'parseDynamicFilePragma' is impure there seems to be no reason
  -- to use it.
  dflags0 <- setDefaultExts <$> GHC.getSessionDynFlags
  let tokens = GHC.getOptions dflags0 (GHC.stringToStringBuffer input) fp
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 tokens
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- | Default runner of 'GHC.Ghc' action in 'IO'.

ghcWrapper :: GHC.Ghc a -> IO a
ghcWrapper
  = GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
  . GHC.runGhc (Just libdir)

-- | Run a 'GHC.P' computation.

runParser
  :: GHC.P a                    -- ^ Computation to run
  -> GHC.DynFlags               -- ^ Dynamic flags
  -> FilePath                   -- ^ Module path
  -> String                     -- ^ Module contents
  -> GHC.ParseResult a          -- ^ Parse result
runParser parser flags filename input = GHC.unP parser parseState
  where
    location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
    buffer = GHC.stringToStringBuffer input
    parseState = GHC.mkPState flags buffer location

-- | Remove GHC style line pragams (@{-# LINE .. #-}@) and convert them into
-- comments.

stripLinePragmas :: FilePath -> String -> (String, [Located String])
stripLinePragmas path = unlines' . unzip . findLines path . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: FilePath -> [String] -> [(String, Maybe (Located String))]
findLines path = zipWith (checkLine path) [1..]

checkLine :: FilePath -> Int -> String -> (String, Maybe (Located String))
checkLine path line s
  | "{-# LINE" `isPrefixOf` s =
      let (pragma, res) = getPragma s
          size = length pragma
          ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (size + 1))
      in (res, Just $ L ss pragma)
  | "#!" `isPrefixOf` s =
      let ss = mkSrcSpan (mkSrcLoc' 1) (mkSrcLoc' (length s))
      in ("",Just $ L ss s)
  | otherwise = (s, Nothing)
  where
    mkSrcLoc' = mkSrcLoc (GHC.mkFastString path) line

getPragma :: String -> (String, String)
getPragma [] = error "Ormolu.Parser.getPragma: input must not be empty"
getPragma s@(x:xs)
  | "#-}" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
      let (prag, remline) = getPragma xs
      in (x:prag, ' ':remline)

-- | Enable all language extensions that we think should be enabled by
-- default for ease of use.

setDefaultExts :: DynFlags -> DynFlags
setDefaultExts flags = foldl' GHC.xopt_set flags autoExts
  where
    autoExts = allExts \\ manualExts
    allExts = [minBound..maxBound]

deriving instance Bounded Extension

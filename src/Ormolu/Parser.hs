{-# LANGUAGE RecordWildCards #-}

-- | Parser for Haskell source code.

module Ormolu.Parser
  ( parseModule
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import GHC hiding (IE, parseModule, parser)
import GHC.LanguageExtensions.Type (Extension (Cpp))
import GHC.Paths (libdir)
import Ormolu.Anns
import Ormolu.CommentStream
import Ormolu.Config
import Ormolu.Exception
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
  => [DynOption]        -- ^ Dynamic options that affect parsing
  -> FilePath           -- ^ File name (only for source location annotations)
  -> String             -- ^ Input for parser
  -> m ( [GHC.Warn]
       , Either (SrcSpan, String)
                (CommentStream, Anns, ParsedSource)
       )
parseModule dynOpts path input' = liftIO $ do
  let (input, extraComments) = stripLinePragmas input'
  (ws, dynFlags) <- ghcWrapper $ do
    dynFlags0 <- initDynFlagsPure path input
    (dynFlags1, _, ws) <-
      GHC.parseDynamicFilePragma dynFlags0 (dynOptionToLocatedStr <$> dynOpts)
    return (ws, dynFlags1)
  -- NOTE It's better to throw this outside of 'ghcWrapper' because
  -- otherwise the exception will be wrapped as a GHC panic, which we don't
  -- want.
  when (GHC.xopt Cpp dynFlags) $
   throwIO OrmoluCppEnabled
  let r = case runParser GHC.parseModule dynFlags path input of
        GHC.PFailed _ ss m ->
          Left (ss, GHC.showSDoc dynFlags m)
        GHC.POk pstate pmod ->
          Right ( mkCommentStream extraComments pstate
                , mkAnns pstate
                , pmod
                )
  return (ws, r)

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
  dflags0 <- GHC.getSessionDynFlags
  let pragmaInfo = GHC.getOptions dflags0 (GHC.stringToStringBuffer input) fp
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 pragmaInfo
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

stripLinePragmas :: String -> (String, [Located String])
stripLinePragmas = unlines' . unzip . findLines . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: [String] -> [(String, Maybe (Located String))]
findLines = zipWith checkLine [1..]

checkLine :: Int -> String -> (String, Maybe (Located String))
checkLine line s
  | "{-# LINE" `isPrefixOf` s =
      let (pragma, res) = getPragma s
          size = length pragma
          mSrcLoc = mkSrcLoc (GHC.mkFastString "LINE")
          ss = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (size+1))
      in (res, Just $ L ss pragma)
  -- Deal with shebang/cpp directives too
  -- x |  "#" `isPrefixOf` s = ("",Just $ Comment ((line, 1), (line, length s)) s)
  | "#!" `isPrefixOf` s =
      let mSrcLoc = mkSrcLoc (GHC.mkFastString "SHEBANG")
          ss = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (length s))
      in ("",Just $ L ss s)
  | otherwise = (s, Nothing)

getPragma :: String -> (String, String)
getPragma [] = error "Ormolu.Parser.getPragma: input must not be empty"
getPragma s@(x:xs)
  | "#-}" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
      let (prag, remline) = getPragma xs
      in (x:prag, ' ':remline)

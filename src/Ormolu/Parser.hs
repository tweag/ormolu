-- | Parser for Haskell source code.

module Ormolu.Parser
  ( parseModule
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import GHC.LanguageExtensions.Type (Extension (Cpp))
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModule)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Config
import Ormolu.Exception
import qualified CmdLineParser as GHC
import qualified DynFlags as GHC
import qualified GHC hiding (parseModule)

-- | Parse a complete module from string.

parseModule
  :: MonadIO m
  => [DynOption]        -- ^ Dynamic options that affect parsing
  -> FilePath           -- ^ File name (only for source location annotations)
  -> String             -- ^ Input for parser
  -> m ([GHC.Warn], Either (GHC.SrcSpan, String) (Anns, GHC.ParsedSource))
parseModule dynOpts path input = liftIO $ do
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
  return (ws, parseModuleFromStringInternal dynFlags path input)

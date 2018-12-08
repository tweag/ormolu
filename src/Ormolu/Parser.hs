module Ormolu.Parser
  ( parseModule )
where

import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModule)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Type
import qualified CmdLineParser as GHC
import qualified DynFlags as GHC
import qualified GHC hiding (parseModule)

-- | Parse a complete module from string.

parseModule
  :: [DynOption]        -- ^ Dynamic options that affect parsing
  -> FilePath           -- ^ File name (only for source location annotations)
  -> String             -- ^ Input for parser
  -> IO ([GHC.Warn], Either (GHC.SrcSpan, String) (Anns, GHC.ParsedSource))
parseModule dynOpts path input = ghcWrapper $ do
  dynFlags0 <- initDynFlagsPure path input
  (dynFlags1, _, ws) <- GHC.parseDynamicFilePragma dynFlags0 (dynOption <$> dynOpts)
  return (ws, parseModuleFromStringInternal dynFlags1 path input)

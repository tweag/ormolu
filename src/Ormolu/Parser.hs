{-# LANGUAGE RecordWildCards #-}

-- | Parser for Haskell source code.

module Ormolu.Parser
  ( parseModule
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics
import GHC hiding (GhcPs, IE, parseModule)
import GHC.LanguageExtensions.Type (Extension (Cpp))
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModule)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Config
import Ormolu.Exception
import qualified CmdLineParser as GHC
import qualified Data.Map.Strict as M
import qualified DynFlags as GHC

-- | Parse a complete module from string.

parseModule
  :: MonadIO m
  => [DynOption]        -- ^ Dynamic options that affect parsing
  -> FilePath           -- ^ File name (only for source location annotations)
  -> String             -- ^ Input for parser
  -> m ([GHC.Warn], Either (SrcSpan, String) (Anns, ParsedSource))
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
  let r = case parseModuleFromStringInternal dynFlags path input of
            Left e -> Left e
            Right (anns, psrc) ->
              Right (dropImportComments anns psrc, psrc)
  return (ws, r)

-- | Drop comments associated with elements of import lists. The reason we
-- do this is that those comments are not associated as user would expect.
--
-- For example:
--
-- > import Foo -- (1)
-- > import Bar -- (2)
-- > import Baz -- (3)
--
-- Here, @(1)@ is considered preceding comment of @Baz@ import, in fact, it
-- has nothing to do with @Foo@. Similarly, @(2)@ is associted with @Baz@,
-- not @Bar@. Finally @(3)@ is actually preceeding comment for the thing
-- that follows the import list, or if nothing follows, it's a trailing
-- comment of the whole module.
--
-- Since we do sorting of imports when we print them, we would need to do
-- sorting of corresponding annotations as well (because we do checking of
-- annotations and AST in "Ormolu.Diff" as part of the self-check), and this
-- is hard. Even if we manage to do that the result will be totally
-- confusing, e.g. for the example above:
--
-- > -- (1)
-- > import Bar
-- > -- (2)
-- > import Baz
-- > import Foo
-- > -- (3)
--
-- The solution is to drop the tricky comments right after parsing, so we
-- don't need to deal with them at all, including the need to update
-- annotations for the correctness checking we do in "Ormolu.Diff".
--
-- The solution is not perfect, but practical.

dropImportComments
  :: Anns
  -> ParsedSource
  -> Anns
dropImportComments anns (L _ HsModule {..}) =
  foldr M.delete anns (hsmodImports >>= keyAnnsFor)

-- | Extract all 'AnnKey's from given 'Data'.

keyAnnsFor :: GenericQ [AnnKey]
keyAnnsFor a = everything mappend (const id `ext2Q` queryLocated) a []
  where
    queryLocated
      :: (Data e0, Data e1)
      => GenLocated e0 e1
      -> [AnnKey]
      -> [AnnKey]
    queryLocated (L mspn x) =
      case cast mspn :: Maybe SrcSpan of
        Nothing -> id
        Just spn -> (mkAnnKey (L spn x) :)

-- | A formatter for Haskell source code.

{-# LANGUAGE RecordWildCards #-}

module Ormolu
  ( ormolu
  , ormoluFile
  , Config (..)
  , defaultConfig
  , DynOption (..)
  , OrmoluException (..)
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Config
import Ormolu.Diff
import Ormolu.Parser
import Ormolu.Printer
import qualified Data.Text as T
import qualified GHC

-- | Format a 'String', return formatted version as 'Text'.
--
-- The function
--
--     * Takes 'String' because that's what GHC parser accepts.
--     * Needs 'IO' because some functions from GHC that are necessary to
--       setup parsing context require 'IO'. There should be no visible
--       side-effects though.
--     * Takes file name just to use it in parse error messages.
--     * Throws 'OrmoluException'.

ormolu
  :: (MonadIO m, MonadThrow m)
  => Config                     -- ^ Ormolu configuration
  -> FilePath                   -- ^ Location of source file
  -> String                     -- ^ Input to format
  -> m Text
ormolu cfg path str = do
  (anns0, parsedSrc0) <-
    parseModule' cfg OrmoluParsingFailed path str
  let txt = printModule anns0 parsedSrc0
  -- Parse the result of pretty-printing again and make sure that AST is the
  -- same as AST of original snippet module span positions.
  (anns1, parsedSrc1) <-
    parseModule' cfg OrmoluOutputParsingFailed "<rendered>" (T.unpack txt)
  when (diff (anns0, parsedSrc0) (anns1, parsedSrc1)) $
    throwM (OrmoluASTDiffers str txt)
  return txt

-- | Load a file and format it. The file stays intact and the rendered
-- version is returned as 'Text'.
--
-- > ormoluFile cfg path =
-- >   liftIO (readFile path) >>= ormolu cfg path

ormoluFile
  :: (MonadIO m, MonadThrow m)
  => Config                     -- ^ Ormolu configuration
  -> FilePath                   -- ^ Location of source file
  -> m Text                     -- ^ Resulting rendition
ormoluFile cfg path =
  liftIO (readFile path) >>= ormolu cfg path

-- | A wrapper around 'parseModule'.

parseModule'
  :: (MonadIO m, MonadThrow m)
  => Config                     -- ^ Ormolu configuration
  -> (GHC.SrcSpan -> String -> OrmoluException)
     -- ^ How to obtain 'OrmoluException' to throw when parsing fails
  -> FilePath                   -- ^ File name to use in errors
  -> String                     -- ^ Actual input for the parser
  -> m (Anns, GHC.ParsedSource) -- ^ Annotations and parsed source
parseModule' Config {..} mkException path str = do
  (_, r) <- liftIO (parseModule cfgDynOptions path str)
  case r of
    Left (spn, err) -> throwM (mkException spn err)
    Right x -> return x

-- | Ormolu exception representing all cases when 'ormoluFile' can fail.

data OrmoluException
  = OrmoluParsingFailed GHC.SrcSpan String
    -- ^ Parsing of original source code failed
  | OrmoluOutputParsingFailed GHC.SrcSpan String
    -- ^ Parsing of formatted source code failed
  | OrmoluASTDiffers String Text
    -- ^ Original and resulting ASTs differ, first argument is the original
    -- source code, second argument is rendered source code
  deriving (Eq, Show)

instance Exception OrmoluException

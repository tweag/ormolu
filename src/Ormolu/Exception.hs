-- | 'OrmoluException' type and surrounding definitions.

{-# LANGUAGE LambdaCase #-}

module Ormolu.Exception
  ( OrmoluException (..)
  , withPrettyOrmoluExceptions
  )
where

import Control.Exception
import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO
import qualified GHC
import qualified Outputable as GHC

-- | Ormolu exception representing all cases when 'ormoluFile' can fail.

data OrmoluException
  = OrmoluCppEnabled
    -- ^ Ormolu does not work with source files that use CPP
  | OrmoluParsingFailed GHC.SrcSpan String
    -- ^ Parsing of original source code failed
  | OrmoluOutputParsingFailed GHC.SrcSpan String
    -- ^ Parsing of formatted source code failed
  | OrmoluASTDiffers String Text
    -- ^ Original and resulting ASTs differ, first argument is the original
    -- source code, second argument is rendered source code
  deriving (Eq, Show)

instance Exception OrmoluException where
  displayException = \case
    OrmoluCppEnabled -> "CPP is not supported"
    OrmoluParsingFailed s e ->
      showParsingErr "parsing of source code failed:" s e
    OrmoluOutputParsingFailed s e ->
      showParsingErr "parsing of formatted code failed (report the bug):" s e
    OrmoluASTDiffers _ _ ->
      "AST of input and AST of formatted code differ, report the bug"

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely using 'displayException'.

withPrettyOrmoluExceptions
  :: IO a                       -- ^ Action that may throw the exception
  -> IO a
withPrettyOrmoluExceptions m = m `catch` h
  where
    h :: OrmoluException -> IO a
    h e = do
      hPutStrLn stderr (displayException e)
      exitFailure

----------------------------------------------------------------------------
-- Helpers

-- | Show a parse error.

showParsingErr :: String -> GHC.SrcSpan -> String -> String
showParsingErr msg spn err = unlines
  [ msg
  , showOutputable spn
  , err
  ]

-- | Display something 'GHC.Outputable'.

showOutputable :: GHC.Outputable o => o -> String
showOutputable = GHC.showSDocUnsafe . GHC.ppr

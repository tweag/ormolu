{-# LANGUAGE LambdaCase #-}

-- | 'OrmoluException' type and surrounding definitions.
module Ormolu.Exception
  ( OrmoluException (..),
    withPrettyOrmoluExceptions,
  )
where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified GHC
import Ormolu.Utils (showOutputable, withIndent)
import qualified Outputable as GHC
import System.Exit (ExitCode (..), exitWith)
import System.IO

-- | Ormolu exception representing all cases when Ormolu can fail.
data OrmoluException
  = -- | Ormolu does not work with source files that use CPP
    OrmoluCppEnabled FilePath
  | -- | Parsing of original source code failed
    OrmoluParsingFailed GHC.SrcSpan String
  | -- | Parsing of formatted source code failed
    OrmoluOutputParsingFailed GHC.SrcSpan String
  | -- | Original and resulting ASTs differ
    OrmoluASTDiffers FilePath [GHC.SrcSpan]
  | -- | Formatted source code is not idempotent
    OrmoluNonIdempotentOutput GHC.RealSrcLoc Text Text
  | -- | Some GHC options were not recognized
    OrmoluUnrecognizedOpts (NonEmpty String)
  deriving (Eq, Show)

instance Exception OrmoluException where
  displayException = \case
    OrmoluCppEnabled path ->
      unlines
        [ "CPP is not supported:",
          withIndent path
        ]
    OrmoluParsingFailed s e ->
      showParsingErr "Parsing of source code failed:" s [e]
    OrmoluOutputParsingFailed s e ->
      showParsingErr "Parsing of formatted code failed:" s [e]
        ++ "Please, consider reporting the bug.\n"
    OrmoluASTDiffers path ss ->
      unlines $
        [ "AST of input and AST of formatted code differ."
        ]
          ++ fmap
            withIndent
            ( case fmap (\s -> "at " ++ showOutputable s) ss of
                [] -> ["in " ++ path]
                xs -> xs
            )
          ++ ["Please, consider reporting the bug."]
    OrmoluNonIdempotentOutput loc left right ->
      showParsingErr
        "Formatting is not idempotent:"
        loc
        ["before: " ++ show left, "after:  " ++ show right]
        ++ "Please, consider reporting the bug.\n"
    OrmoluUnrecognizedOpts opts ->
      unlines
        [ "The following GHC options were not recognized:",
          (withIndent . unwords . NE.toList) opts
        ]

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely using 'displayException'.
withPrettyOrmoluExceptions ::
  -- | Action that may throw the exception
  IO a ->
  IO a
withPrettyOrmoluExceptions m = m `catch` h
  where
    h :: OrmoluException -> IO a
    h e = do
      hPutStrLn stderr (displayException e)
      exitWith . ExitFailure $
        case e of
          -- Error code 1 is for `error` or `notImplemented`
          OrmoluCppEnabled {} -> 2
          OrmoluParsingFailed {} -> 3
          OrmoluOutputParsingFailed {} -> 4
          OrmoluASTDiffers {} -> 5
          OrmoluNonIdempotentOutput {} -> 6
          OrmoluUnrecognizedOpts {} -> 7

----------------------------------------------------------------------------
-- Helpers

-- | Show a parse error.
showParsingErr :: GHC.Outputable a => String -> a -> [String] -> String
showParsingErr msg spn err =
  unlines $
    [ msg,
      withIndent (showOutputable spn)
    ]
      ++ map withIndent err

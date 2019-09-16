{-# LANGUAGE LambdaCase #-}

-- | 'OrmoluException' type and surrounding definitions.

module Ormolu.Exception
  ( OrmoluException (..)
  , withPrettyOrmoluExceptions
  )
where

import Control.Exception
import Data.Text (Text)
import Ormolu.Utils (showOutputable)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import qualified GHC
import qualified Outputable as GHC

-- | Ormolu exception representing all cases when Ormolu can fail.

data OrmoluException
  = OrmoluCppEnabled FilePath
    -- ^ Ormolu does not work with source files that use CPP
  | OrmoluParsingFailed GHC.SrcSpan String
    -- ^ Parsing of original source code failed
  | OrmoluOutputParsingFailed GHC.SrcSpan String
    -- ^ Parsing of formatted source code failed
  | OrmoluASTDiffers FilePath [GHC.SrcSpan]
    -- ^ Original and resulting ASTs differ
  | OrmoluNonIdempotentOutput GHC.RealSrcLoc Text Text
    -- ^ Formatted source code is not idempotent
  deriving (Eq, Show)

instance Exception OrmoluException where
  displayException = \case
    OrmoluCppEnabled path -> unlines
      [ "CPP is not supported:"
      , withIndent path
      ]
    OrmoluParsingFailed s e ->
      showParsingErr "Parsing of source code failed:" s [e]
    OrmoluOutputParsingFailed s e ->
      showParsingErr "Parsing of formatted code failed:" s [e] ++
        "Please, consider reporting the bug.\n"
    OrmoluASTDiffers path ss -> unlines $
      [ "AST of input and AST of formatted code differ."
      ]
      ++ (fmap withIndent $ case fmap (\s -> "at " ++ showOutputable s) ss of
           [] -> ["in " ++ path]
           xs -> xs) ++
      [ "Please, consider reporting the bug." ]
    OrmoluNonIdempotentOutput loc left right ->
      showParsingErr "Formatting is not idempotent:" loc
        [ "before: " ++ show left , "after:  " ++ show right ]
        ++ "Please, consider reporting the bug.\n"

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
      exitWith . ExitFailure $
        case e of
          -- Error code 1 is for `error` or `notImplemented`
          OrmoluCppEnabled _ -> 2
          OrmoluParsingFailed _ _ -> 3
          OrmoluOutputParsingFailed _ _ -> 4
          OrmoluASTDiffers _ _ -> 5
          OrmoluNonIdempotentOutput _ _ _ -> 6

----------------------------------------------------------------------------
-- Helpers

-- | Show a parse error.

showParsingErr :: GHC.Outputable a => String -> a -> [String] -> String
showParsingErr msg spn err = unlines $
  [ msg
  , withIndent (showOutputable spn)
  ] ++ map withIndent err

-- | Indent with 2 spaces for readability.

withIndent :: String -> String
withIndent txt = "  " ++ txt

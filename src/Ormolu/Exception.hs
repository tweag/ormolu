{-# LANGUAGE LambdaCase #-}

-- | 'OrmoluException' type and surrounding definitions.

module Ormolu.Exception
  ( OrmoluException (..)
  , withPrettyOrmoluExceptions
  , SemicolonWarning (..)
  )
where

import Control.Exception
import Ormolu.Utils (showOutputable)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import qualified GHC

-- | Ormolu exception representing all cases when Ormolu can fail.

data OrmoluException
  = OrmoluCppEnabled FilePath
    -- ^ Ormolu does not work with source files that use CPP
  | OrmoluParsingFailed GHC.SrcSpan String
    -- ^ Parsing of original source code failed
  | OrmoluOutputParsingFailed (Maybe SemicolonWarning) GHC.SrcSpan String
    -- ^ Parsing of formatted source code failed
  | OrmoluASTDiffers (Maybe SemicolonWarning) FilePath [GHC.SrcSpan]
    -- ^ Original and resulting ASTs differ
  deriving (Eq, Show)

instance Exception OrmoluException where
  displayException = \case
    OrmoluCppEnabled path -> unlines
      [ "CPP is not supported:"
      , withIndent path
      ]
    OrmoluParsingFailed s e ->
      showParsingErr "Parsing of source code failed:" s e
    OrmoluOutputParsingFailed w s e ->
      showParsingErr "Parsing of formatted code failed:" s e ++
        (maybe "" (\sw -> showSemicolonWarning sw ++ "\n") w) ++
        "Please, consider reporting the bug.\n"
    OrmoluASTDiffers w path ss -> unlines $
      [ "AST of input and AST of formatted code differ."
      ]
      ++ (fmap withIndent $ case fmap (\s -> "at " ++ showOutputable s) ss of
           [] -> ["in " ++ path]
           xs -> xs)
      ++ maybe [] (pure . showSemicolonWarning) w
      ++ [ "Please, consider reporting the bug."
         ]

-- | A common failure case is about the inputs with semicolons.
-- So we optionally attach semicolon usages to the exceptions.

data SemicolonWarning =
  SemicolonWarning [Int] -- ^ List of lines with at least one semicolon
  deriving (Eq, Show)

showSemicolonWarning :: SemicolonWarning -> String
showSemicolonWarning (SemicolonWarning xs) = mconcat
  [ "Note that not all usages of semicolons are supported at the moment.\n"
  , "  (semicolons found at lines: ", show xs, ")"
  ]

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
          OrmoluOutputParsingFailed _ _ _ -> 4
          OrmoluASTDiffers _ _ _ -> 5

----------------------------------------------------------------------------
-- Helpers

-- | Show a parse error.

showParsingErr :: String -> GHC.SrcSpan -> String -> String
showParsingErr msg spn err = unlines
  [ msg
  , withIndent (showOutputable spn)
  , withIndent err
  ]

-- | Indent with 2 spaces for readability.

withIndent :: String -> String
withIndent txt = "  " ++ txt

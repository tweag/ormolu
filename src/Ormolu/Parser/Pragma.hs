-- | A module for parsing of pragmas from comments.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Parser.Pragma
  ( Pragma (..)
  , parsePragma
  )
where

import Control.Monad
import Data.Char (toLower, isSpace)
import Data.List
import FastString (mkFastString, unpackFS)
import Module (newSimpleUnitId, ComponentId (..))
import SrcLoc
import StringBuffer
import qualified EnumSet as ES
import qualified Lexer as L

-- | Ormolu's representation of pragmas.

data Pragma
  = PragmaLanguage [String]     -- ^ Language pragma
  | PragmaOptionsGHC String     -- ^ GHC options pragma
  | PragmaOptionsHaddock String -- ^ Haddock options pragma
  deriving (Show, Eq)

-- | Extract a pragma from a comment if possible, or return 'Nothing'
-- otherwise.

parsePragma
  :: String                     -- ^ Comment to try to parse
  -> Maybe Pragma
parsePragma input = do
  inputNoPrefix <- stripPrefix "{-#" input
  guard ("#-}" `isSuffixOf` input)
  let contents = take (length inputNoPrefix - 3) inputNoPrefix
      (pragmaName, cs) = (break isSpace . dropWhile isSpace) contents
  case toLower <$> pragmaName of
    "language" -> PragmaLanguage <$> parseExtensions cs
    "options_ghc" -> Just $ PragmaOptionsGHC (trimSpaces cs)
    "options_haddock" -> Just $ PragmaOptionsHaddock (trimSpaces cs)
    _ -> Nothing
  where
    trimSpaces :: String -> String
    trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

-- | Assuming the input consists of a series of tokens from a language
-- pragma, return the set of enabled extensions.

parseExtensions :: String  -> Maybe [String]
parseExtensions str = tokenize str >>= go
  where
    go = \case
      (L.ITconid ext : []) -> return [unpackFS ext]
      (L.ITconid ext : L.ITcomma : xs) -> (unpackFS ext :) <$> go xs
      _ -> Nothing

-- | Tokenize a given input using GHC's lexer.

tokenize :: String -> Maybe [L.Token]
tokenize input =
  case L.unP pLexer parseState of
    L.PFailed {} -> Nothing
    L.POk _ x -> Just x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = stringToStringBuffer input
    parseState = L.mkPStatePure parserFlags buffer location
    parserFlags = L.ParserFlags
      { L.pWarningFlags = ES.empty
      , L.pExtensionFlags = ES.empty
      , L.pThisPackage = newSimpleUnitId (ComponentId (mkFastString ""))
      , L.pExtsBitmap = 0xffffffffffffffff
      }

-- | Haskell lexer.

pLexer :: L.P [L.Token]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case unLoc r of
        L.ITeof -> return []
        x       -> (x:) <$> go

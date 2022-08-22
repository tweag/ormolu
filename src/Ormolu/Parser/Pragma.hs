{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for parsing of pragmas from comments.
module Ormolu.Parser.Pragma
  ( Pragma (..),
    parsePragma,
  )
where

import Control.Monad
import Data.Char (isSpace, toLower)
import qualified Data.List as L
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.DynFlags (baseDynFlags)
import qualified GHC.Parser.Lexer as L
import GHC.Types.SrcLoc

-- | Ormolu's representation of pragmas.
data Pragma
  = -- | Language pragma
    PragmaLanguage [String]
  | -- | GHC options pragma
    PragmaOptionsGHC String
  | -- | Haddock options pragma
    PragmaOptionsHaddock String
  deriving (Show, Eq)

-- | Extract a pragma from a comment if possible, or return 'Nothing'
-- otherwise.
parsePragma ::
  -- | Comment to try to parse
  String ->
  Maybe Pragma
parsePragma input = do
  inputNoPrefix <- L.stripPrefix "{-#" input
  guard ("#-}" `L.isSuffixOf` input)
  let contents = take (length inputNoPrefix - 3) inputNoPrefix
      (pragmaName, cs) = (break isSpace . dropWhile isSpace) contents
  case toLower <$> pragmaName of
    "language" -> PragmaLanguage <$> parseExtensions cs
    "options_ghc" -> Just $ PragmaOptionsGHC (trimSpaces cs)
    "options_haddock" -> Just $ PragmaOptionsHaddock (trimSpaces cs)
    _ -> Nothing
  where
    trimSpaces :: String -> String
    trimSpaces = L.dropWhileEnd isSpace . dropWhile isSpace

-- | Assuming the input consists of a series of tokens from a language
-- pragma, return the set of enabled extensions.
parseExtensions :: String -> Maybe [String]
parseExtensions str = tokenize str >>= go
  where
    go = \case
      [L.ITconid ext] -> return [unpackFS ext]
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
    parseState = L.initParserState parserOpts buffer location
    parserOpts = initParserOpts baseDynFlags

-- | Haskell lexer.
pLexer :: L.P [L.Token]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case unLoc r of
        L.ITeof -> return []
        x -> (x :) <$> go

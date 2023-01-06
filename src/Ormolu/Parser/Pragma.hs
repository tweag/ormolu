{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for parsing of pragmas from comments.
module Ormolu.Parser.Pragma
  ( Pragma (..),
    parsePragma,
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Data.FastString (bytesFS, mkFastString)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.DynFlags (baseDynFlags)
import qualified GHC.Parser.Lexer as L
import GHC.Types.SrcLoc
import Ormolu.Utils (textToStringBuffer)

-- | Ormolu's representation of pragmas.
data Pragma
  = -- | Language pragma
    PragmaLanguage [Text]
  | -- | GHC options pragma
    PragmaOptionsGHC Text
  | -- | Haddock options pragma
    PragmaOptionsHaddock Text
  deriving (Show, Eq)

-- | Extract a pragma from a comment if possible, or return 'Nothing'
-- otherwise.
parsePragma ::
  -- | Comment to try to parse
  Text ->
  Maybe Pragma
parsePragma input = do
  contents <- T.stripSuffix "#-}" =<< T.stripPrefix "{-#" input
  let (pragmaName, cs) = (T.break isSpace . T.dropWhile isSpace) contents
  case T.toLower pragmaName of
    "language" -> PragmaLanguage <$> parseExtensions cs
    "options_ghc" -> Just $ PragmaOptionsGHC (T.strip cs)
    "options_haddock" -> Just $ PragmaOptionsHaddock (T.strip cs)
    _ -> Nothing

-- | Assuming the input consists of a series of tokens from a language
-- pragma, return the set of enabled extensions.
parseExtensions :: Text -> Maybe [Text]
parseExtensions str = tokenize str >>= go
  where
    go = \case
      [L.ITconid ext] -> return [fsToText ext]
      (L.ITconid ext : L.ITcomma : xs) -> (fsToText ext :) <$> go xs
      _ -> Nothing
    fsToText = T.decodeUtf8 . bytesFS

-- | Tokenize a given input using GHC's lexer.
tokenize :: Text -> Maybe [L.Token]
tokenize input =
  case L.unP pLexer parseState of
    L.PFailed {} -> Nothing
    L.POk _ x -> Just x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = textToStringBuffer input
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

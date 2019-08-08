-- | A module for parsing of language pragmas from comments.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Parser.LanguagePragma
  ( parseLanguagePragma
  )
where

import Control.Monad
import Data.Char (toLower)
import Data.List (stripPrefix, isSuffixOf)
import Data.Set (Set)
import FastString (mkFastString, unpackFS)
import Module (newSimpleUnitId, ComponentId (..))
import SrcLoc
import StringBuffer
import qualified Data.Set as S
import qualified EnumSet as ES
import qualified Lexer as L

-- | Extract a 'Set' of language extensions from a comment if it's a
-- language pragma or return 'Nothing' otherwise.

parseLanguagePragma
  :: String                     -- ^ Comment to try to parse
  -> Maybe (Set String)
parseLanguagePragma input = do
  inputNoPrefix <- stripPrefix "{-#" input
  guard ("#-}" `isSuffixOf` input)
  let contents = take (length inputNoPrefix - 3) inputNoPrefix
  toks0 <- tokenize contents
  toks1 <- dropLanguage toks0
  S.fromList <$> parseExtensions toks1

-- | Assuming the input consists of a series of tokens from a language
-- pragma, return the set of enabled extensions.

parseExtensions :: [L.Token] -> Maybe [String]
parseExtensions = \case
  (L.ITconid ext : []) -> return [unpackFS ext]
  (L.ITconid ext : L.ITcomma : xs) -> (unpackFS ext :) <$> parseExtensions xs
  _ -> Nothing

-- | Drop LANGUAGE pragma text from the token stream.

dropLanguage :: [L.Token] -> Maybe [L.Token]
dropLanguage = \case
  (L.ITconid lang : xs) -> go lang xs
  (L.ITvarid lang : xs) -> go lang xs
  _ -> Nothing
  where
    go lang xs =
      if (toLower <$> unpackFS lang) == "language"
        then Just xs
        else Nothing

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

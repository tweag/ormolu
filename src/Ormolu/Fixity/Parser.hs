{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parser for fixity maps.
module Ormolu.Fixity.Parser
  ( parseFixityOverrides,
    parseFixityDeclaration,

    -- * Raw parsers
    pFixity,
    pOperator,
  )
where

import Control.Monad (when)
import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Ormolu.Fixity
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | Parse textual representation of 'FixityOverrides'.
parseFixityOverrides ::
  -- | Location of the file we are parsing (only for parse errors)
  FilePath ->
  -- | File contents to parse
  Text ->
  -- | Parse result
  Either (ParseErrorBundle Text Void) FixityOverrides
parseFixityOverrides = runParser pFixityOverrides

-- | Parse a single self-contained fixity declaration.
parseFixityDeclaration ::
  -- | Expression to parse
  Text ->
  -- | Parse result
  Either (ParseErrorBundle Text Void) [(OpName, FixityInfo)]
parseFixityDeclaration = runParser (pFixity <* eof) ""

pFixityOverrides :: Parser FixityOverrides
pFixityOverrides =
  FixityOverrides . Map.fromList . mconcat
    <$> many (pFixity <* eol <* hidden space)
    <* eof

-- | Parse a single fixity declaration, such as
--
-- > infixr 4 +++, >>>
pFixity :: Parser [(OpName, FixityInfo)]
pFixity = do
  fiDirection <- pFixityDirection
  hidden hspace1
  offsetAtPrecedence <- getOffset
  fiPrecedence <- L.decimal
  when (fiPrecedence > 9) $
    region
      (setErrorOffset offsetAtPrecedence)
      (fail "precedence should not be greater than 9")
  hidden hspace1
  ops <- sepBy1 pOperator (char ',' >> hidden hspace)
  hidden hspace
  let fixityInfo = FixityInfo {..}
  return ((,fixityInfo) <$> ops)

pFixityDirection :: Parser FixityDirection
pFixityDirection =
  choice
    [ InfixL <$ string "infixl",
      InfixR <$ string "infixr",
      InfixN <$ string "infix"
    ]

-- | See <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html>
pOperator :: Parser OpName
pOperator = OpName <$> (tickedOperator <|> normalOperator)
  where
    tickedOperator = between tick tick haskellIdentifier
    tick = char '`'
    haskellIdentifier =
      T.cons
        <$> letterChar
        <*> takeWhileP Nothing (\x -> Char.isAlphaNum x || x == '_' || x == '\'')
    normalOperator =
      takeWhile1P (Just "operator character") $ \x ->
        (Char.isSymbol x || Char.isPunctuation x)
          && (x /= ',' && x /= '`' && x /= '(' && x /= ')')

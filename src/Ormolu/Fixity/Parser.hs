{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Parser for fixity maps.
module Ormolu.Fixity.Parser
  ( parseFixityMap,
    parseFixityDeclaration,

    -- * Raw parsers
    pFixity,
    pOperator,
  )
where

import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Void (Void)
import Ormolu.Fixity.Internal
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Parse textual representation of a 'FixityMap'.
parseFixityMap ::
  -- | Location of the file we are parsing (only for parse errors)
  FilePath ->
  -- | File contents to parse
  Text ->
  -- | Parse result
  Either (ParseErrorBundle Text Void) FixityMap
parseFixityMap = runParser pFixityMap

-- | Parse a single self-contained fixity declaration.
parseFixityDeclaration ::
  -- | Expression to parse
  Text ->
  -- | Parse result
  Either (ParseErrorBundle Text Void) [(String, FixityInfo)]
parseFixityDeclaration = runParser (pFixity <* eof) ""

pFixityMap :: Parser FixityMap
pFixityMap =
  Map.fromListWith (<>) . mconcat
    <$> many (pFixity <* newline <* hidden space)
    <* eof

-- | Parse a single fixity declaration, such as
--
-- > infixr 4 +++, >>>
pFixity :: Parser [(String, FixityInfo)]
pFixity = do
  fiDirection <- Just <$> pFixityDirection
  hidden hspace1
  fiMinPrecedence <- L.decimal
  let fiMaxPrecedence = fiMinPrecedence
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
pOperator :: Parser String
pOperator = tickedOperator <|> normalOperator
  where
    tickedOperator = between tick tick haskellIdentifier
    tick = char '`'
    haskellIdentifier = do
      x <- letterChar
      xs <- many (alphaNumChar <|> char '_' <|> char '\'')
      return (x : xs)
    normalOperator = some operatorChar
    operatorChar =
      satisfy
        (\x -> (Char.isSymbol x || Char.isPunctuation x) && isNotExcluded x)
        <?> "operator character"
      where
        isNotExcluded x = x /= ',' && x /= '`' && x /= '(' && x /= ')'

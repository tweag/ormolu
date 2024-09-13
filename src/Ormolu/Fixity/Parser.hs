{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parser for fixity maps.
module Ormolu.Fixity.Parser
  ( parseDotOrmolu,
    parseFixityDeclaration,
    parseModuleReexportDeclaration,

    -- * Raw parsers
    pFixity,
    pOperator,
    pModuleName,
    pPackageName,

    -- * Internal
    isIdentifierFirstChar,
    isIdentifierConstituent,
    isOperatorConstituent,
    isPackageNameConstituent,
    isModuleSegmentFirstChar,
    isModuleSegmentConstituent,
  )
where

import Control.Monad (void, when)
import Data.Bifunctor (bimap)
import Data.Char (isAlphaNum, isUpper)
import Data.Char qualified as Char
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Distribution.ModuleName (ModuleName)
import Distribution.ModuleName qualified as ModuleName
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Ormolu.Fixity
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- TODO Support fixity namespacing?
-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst
--
-- Note that currently, our fixity machinery does *not* do any namespacing:
--
--  - https://github.com/tweag/ormolu/pull/994#pullrequestreview-1396958951
--    brought this up in the past
--
--  - https://github.com/tweag/ormolu/pull/1029#issue-1718217029
--    has a concrete example (morley-prelude) where namespacing would matter

-- | Parse textual representation of 'FixityOverrides'.
parseDotOrmolu ::
  -- | Location of the file we are parsing (only for parse errors)
  FilePath ->
  -- | File contents to parse
  Text ->
  -- | Parse result
  Either (ParseErrorBundle Text Void) (FixityOverrides, ModuleReexports)
parseDotOrmolu = runParser pDotOrmolu

-- | Parse a single self-contained fixity declaration.
parseFixityDeclaration ::
  -- | Text to parse
  Text ->
  -- | Parse result
  Either (ParseErrorBundle Text Void) [(OpName, FixityInfo)]
parseFixityDeclaration = runParser (pFixity <* eof) ""

-- | Parse a single self-contained module re-export declaration.
parseModuleReexportDeclaration ::
  -- | Text to parse
  Text ->
  -- | Parse result
  Either
    (ParseErrorBundle Text Void)
    (ModuleName, NonEmpty (Maybe PackageName, ModuleName))
parseModuleReexportDeclaration = runParser (pModuleReexport <* eof) ""

pDotOrmolu :: Parser (FixityOverrides, ModuleReexports)
pDotOrmolu =
  bimap
    (FixityOverrides . Map.fromList . mconcat)
    (ModuleReexports . Map.map NE.sort . Map.fromListWith (<>))
    . partitionEithers
    <$> (configLine `sepEndBy` (void eol *> hidden space))
    <* eof
  where
    configLine = eitherP pFixity pModuleReexport

-- | Parse a single fixity declaration, such as
--
-- > infixr 4 +++, >>>
pFixity :: Parser [(OpName, FixityInfo)]
pFixity = do
  fiDirection <- pFixityDirection
  hidden hspace1
  offsetAtPrecedence <- getOffset
  fiPrecedence <-
    try L.float
      <|> (fromIntegral <$> (L.decimal :: Parser Integer))
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
        <*> takeWhileP Nothing isIdentifierConstituent
    normalOperator =
      takeWhile1P (Just "operator character") isOperatorConstituent

pModuleReexport :: Parser (ModuleName, NonEmpty (Maybe PackageName, ModuleName))
pModuleReexport = do
  void (string "module")
  hidden hspace1
  exportingModule <- pModuleName
  hidden hspace1
  void (string "exports")
  hidden hspace1
  mexportedPackage <-
    optional $
      between (char '\"') (char '\"') pPackageName <* hidden hspace1
  exportedModule <- pModuleName
  hidden hspace
  return (exportingModule, NE.singleton (mexportedPackage, exportedModule))

pModuleName :: Parser ModuleName
pModuleName =
  ModuleName.fromString . intercalate "."
    <$> sepBy1 pModuleSegment (char '.')
    <?> "module name"
  where
    pModuleSegment = do
      x <- satisfy isModuleSegmentFirstChar <?> "capital letter"
      xs <-
        many
          ( satisfy isModuleSegmentConstituent
              <?> "module segment continuation"
          )
      return (x : xs)

pPackageName :: Parser PackageName
pPackageName =
  mkPackageName <$> some (satisfy isPackageNameConstituent) <?> "package name"

-- Internal predicates (exposed for testing)

isIdentifierFirstChar :: Char -> Bool
isIdentifierFirstChar = Char.isLetter

isIdentifierConstituent :: Char -> Bool
isIdentifierConstituent x = Char.isAlphaNum x || x == '_' || x == '\''

isOperatorConstituent :: Char -> Bool
isOperatorConstituent x =
  (Char.isSymbol x || Char.isPunctuation x)
    && (x /= ',' && x /= '`' && x /= '(' && x /= ')')

isPackageNameConstituent :: Char -> Bool
isPackageNameConstituent x = x == '-' || isAlphaNum x

isModuleSegmentFirstChar :: Char -> Bool
isModuleSegmentFirstChar x = isAlphaNum x && isUpper x

isModuleSegmentConstituent :: Char -> Bool
isModuleSegmentConstituent x =
  x == '_' || x == '\'' || isAlphaNum x

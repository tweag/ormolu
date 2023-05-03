{-# LANGUAGE OverloadedStrings #-}

-- | Parse Hoogle txt files.
module Hoogle
  ( Package (..),
    Module (..),
    Declaration (..),
    parsePackage,
  )
where

import Control.Monad (void)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Void (Void)
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName (PackageName)
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | Parsed Hoogle package.
data Package = Package
  { packageName :: PackageName,
    modules :: [Module]
  }
  deriving (Eq, Show)

-- | Parsed Hoogle module.
data Module = Module
  { hmModuleName :: ModuleName,
    hmDeclarations :: [Declaration]
  }
  deriving (Eq, Show)

-- | The types of declarations in the Hoogle files we are interested in.
data Declaration
  = Symbol OpName
  | Fixity OpName FixityInfo
  deriving (Eq, Show)

-- | Parse Hoogle package file.
parsePackage ::
  -- | File name
  FilePath ->
  -- | Text to parse
  Text ->
  Either (ParseErrorBundle Text Void) Package
parsePackage = parse pPackage

-- | Parse a package. It starts with the word @\@package@ followed by the
-- name of the package. A package contains zero or more modules.
pPackage :: Parser Package
pPackage = do
  void (skipManyTill (pLineWithoutEol <* eol) (string "@package"))
  hspace1
  packageName <- pPackageName
  hspace
  void eol
  skipManyTill
    (pLineWithoutEol <* eol)
    (lookAhead (void (string "module ") <|> eof))
  modules <- many pModule
  eof
  return (Package packageName modules)

-- | Match a module declaration. It starts with the word @module@ followed
-- by one or more spaces and a module identifier. A module contains
-- 'pDeclaration's and any other arbitrary lines.
pModule :: Parser Module
pModule = do
  void (string "module")
  hspace1
  moduleName <- pModuleName
  hspace
  void eol
  declarations <- mconcat <$> sepEndBy pDeclaration eol
  return (Module moduleName declarations)

-- | Here we are interested in two kinds of declarations:
--
-- > Symbol declaration, e.g.:
-- > ($) :: (a -> b) -> a -> b
-- >
-- > Fixity declaration, e.g.:
-- > infixr 0 $
--
-- We discard everything else while being careful to stop in front of the
-- beginning of a new module.
pDeclaration :: Parser [Declaration]
pDeclaration =
  asum
    [ fmap (uncurry Fixity) <$> try pFixity,
      pure . Symbol <$> try pSymbolDecl,
      [] <$ (notFollowedBy (string "module ") *> pLineWithoutEol)
    ]
  where
    pSymbolDecl = do
      void (string "(")
      r <- pOperator
      void (string ") :: ")
      r <$ pLineWithoutEol

pLineWithoutEol :: Parser ()
pLineWithoutEol = void (takeWhileP Nothing (/= '\n'))

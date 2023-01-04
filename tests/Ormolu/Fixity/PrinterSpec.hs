{-# LANGUAGE RecordWildCards #-}

module Ormolu.Fixity.PrinterSpec (spec) where

import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Ormolu.Fixity.Printer
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck

newtype FixityMapWrapper = FixityMapWrapper FixityMap
  deriving (Show)

instance Arbitrary FixityMapWrapper where
  arbitrary =
    FixityMapWrapper . Map.fromListWith (<>)
      <$> listOf ((,) <$> genOperator <*> genFixityInfo)
    where
      scaleDown = scale (`div` 4)
      genOperator = oneof [genNormalOperator, genIdentifier]
      genNormalOperator =
        listOf1 (scaleDown arbitrary `suchThat` isOperatorConstituent)
      isOperatorConstituent x =
        (Char.isSymbol x || Char.isPunctuation x) && x `notElem` ",`()"
      genIdentifier = do
        x <- arbitrary `suchThat` Char.isLetter
        xs <- listOf1 (scaleDown arbitrary `suchThat` isIdentifierConstituent)
        return (x : xs)
      isIdentifierConstituent x = Char.isAlphaNum x || x == '_' || x == '\''
      genFixityInfo = do
        fiDirection <-
          elements
            [ Nothing,
              Just InfixL,
              Just InfixR,
              Just InfixN
            ]
        fiMinPrecedence <- chooseInt (0, 9)
        fiMaxPrecedence <- chooseInt (0, 9) `suchThat` (>= fiMinPrecedence)
        return FixityInfo {..}

spec :: Spec
spec = do
  describe "parseFixityMap & printFixityMap" $
    it "arbitrary fixity maps are printed and parsed back correctly" $
      property $ \(FixityMapWrapper fixityMap) ->
        parseFixityMap "" (printFixityMap fixityMap) `shouldParse` fixityMap

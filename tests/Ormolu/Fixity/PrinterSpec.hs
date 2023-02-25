{-# LANGUAGE RecordWildCards #-}

module Ormolu.Fixity.PrinterSpec (spec) where

import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Ormolu.Fixity.Printer
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck

newtype FixityMapWrapper = FixityMapWrapper FixityOverrides
  deriving (Show)

instance Arbitrary FixityMapWrapper where
  arbitrary =
    FixityMapWrapper . FixityOverrides . Map.fromList
      <$> listOf ((,) <$> genOperator <*> genFixityInfo)
    where
      scaleDown = scale (`div` 4)
      genOperator =
        OpName . T.pack <$> oneof [genNormalOperator, genIdentifier]
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
            [ InfixL,
              InfixR,
              InfixN
            ]
        fiPrecedence <- chooseInt (0, 9)
        return FixityInfo {..}

spec :: Spec
spec = do
  describe "parseFixityOverrides & printFixityOverrides" $
    it "arbitrary fixity maps are printed and parsed back correctly" $
      property $ \(FixityMapWrapper fixityMap) ->
        parseFixityOverrides "" (printFixityOverrides fixityMap) `shouldParse` fixityMap

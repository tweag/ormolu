{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ormolu.Fixity.PrinterSpec (spec) where

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Distribution.ModuleName (ModuleName)
import Distribution.ModuleName qualified as ModuleName
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Ormolu.Fixity.Printer
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck

instance Arbitrary FixityOverrides where
  arbitrary =
    FixityOverrides . Map.fromList
      <$> listOf ((,) <$> genOperator <*> genFixityInfo)
    where
      genOperator =
        OpName . T.pack <$> oneof [genNormalOperator, genIdentifier]
      genNormalOperator =
        listOf1 (scaleDown arbitrary `suchThat` isOperatorConstituent)
      genIdentifier = do
        x <- arbitrary `suchThat` isIdentifierFirstChar
        xs <- listOf1 (scaleDown arbitrary `suchThat` isIdentifierConstituent)
        return (x : xs)
      genFixityInfo = do
        fiDirection <-
          elements
            [ InfixL,
              InfixR,
              InfixN
            ]
        fiPrecedence <- chooseInt (0, 9)
        return FixityInfo {..}

instance Arbitrary ModuleReexports where
  arbitrary = ModuleReexports . Map.fromListWith combine <$> listOf genReexport
    where
      combine x y = NE.sort (x <> y)
      genReexport = do
        exportingModule <- arbitrary
        exports <- NE.sort . NE.fromList . getNonEmpty <$> scaleDown arbitrary
        return (exportingModule, exports)

instance Arbitrary PackageName where
  arbitrary =
    mkPackageName
      <$> listOf1 (scaleDown arbitrary `suchThat` isPackageNameConstituent)

instance Arbitrary ModuleName where
  arbitrary =
    ModuleName.fromString . intercalate "." <$> scaleDown (listOf1 genSegment)
    where
      genSegment = do
        x <- arbitrary `suchThat` isModuleSegmentFirstChar
        xs <- listOf (arbitrary `suchThat` isModuleSegmentConstituent)
        return (x : xs)

scaleDown :: Gen a -> Gen a
scaleDown = scale (`div` 4)

spec :: Spec
spec = do
  describe "parseFixityOverrides & printFixityOverrides" $
    it "arbitrary fixity maps are printed and parsed back correctly" $
      property $ \fixityOverrides moduleReexports ->
        parseDotOrmolu "" (printDotOrmolu fixityOverrides moduleReexports)
          `shouldParse` (fixityOverrides, moduleReexports)

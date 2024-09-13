{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Printer for fixity overrides.
module Ormolu.Fixity.Printer
  ( printDotOrmolu,
  )
where

import Data.Char qualified as Char
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as B
import Data.Text.Lazy.Builder.RealFloat qualified as B
import Distribution.ModuleName (ModuleName)
import Distribution.ModuleName qualified as ModuleName
import Distribution.Types.PackageName
import Ormolu.Fixity

-- | Print out a textual representation of an @.ormolu@ file.
printDotOrmolu ::
  FixityOverrides ->
  ModuleReexports ->
  Text
printDotOrmolu
  (FixityOverrides fixityOverrides)
  (ModuleReexports moduleReexports) =
    TL.toStrict . B.toLazyText $
      (mconcat . fmap renderSingleFixityOverride . Map.toList) fixityOverrides
        <> (mconcat . fmap renderSingleModuleReexport . Map.toList) moduleReexports

renderSingleFixityOverride :: (OpName, FixityInfo) -> Builder
renderSingleFixityOverride (OpName operator, FixityInfo {..}) =
  mconcat
    [ case fiDirection of
        InfixL -> "infixl"
        InfixR -> "infixr"
        InfixN -> "infix",
      " ",
      renderPrecedence fiPrecedence,
      " ",
      if isTickedOperator operator
        then "`" <> B.fromText operator <> "`"
        else B.fromText operator,
      "\n"
    ]
  where
    isTickedOperator = maybe True (Char.isLetter . fst) . T.uncons

renderSingleModuleReexport ::
  (ModuleName, NonEmpty (Maybe PackageName, ModuleName)) ->
  Builder
renderSingleModuleReexport (exportingModule, exports) =
  sconcat (renderSingle <$> exports)
  where
    renderSingle (mexportedPackage, exportedModule) =
      mconcat
        [ "module ",
          renderModuleName exportingModule,
          " exports ",
          renderOptionalPackageName mexportedPackage,
          renderModuleName exportedModule,
          "\n"
        ]
    renderOptionalPackageName = \case
      Nothing -> mempty
      Just x -> "\"" <> B.fromString (unPackageName x) <> "\" "

renderModuleName :: ModuleName -> Builder
renderModuleName = B.fromString . intercalate "." . ModuleName.components

-- | Render precedence using integer representation for whole numbers.
renderPrecedence :: Double -> Builder
renderPrecedence x =
  let (n :: Int, fraction :: Double) = properFraction x
      isWholeEnough = fraction < 0.0001
   in if isWholeEnough
        then B.decimal n
        else B.realFloat x

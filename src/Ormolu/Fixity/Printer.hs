{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Printer for fixity overrides.
module Ormolu.Fixity.Printer
  ( printFixityOverrides,
  )
where

import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as B
import Ormolu.Fixity

-- | Print out a textual representation of 'FixityOverrides'.
printFixityOverrides :: FixityOverrides -> Text
printFixityOverrides (FixityOverrides m) =
  TL.toStrict
    . B.toLazyText
    . mconcat
    . fmap renderOne
    $ Map.toList m
  where
    renderOne :: (OpName, FixityInfo) -> Builder
    renderOne (OpName operator, FixityInfo {..}) =
      mconcat
        [ case fiDirection of
            InfixL -> "infixl"
            InfixR -> "infixr"
            InfixN -> "infix",
          " ",
          B.decimal fiPrecedence,
          " ",
          if isTickedOperator operator
            then "`" <> B.fromText operator <> "`"
            else B.fromText operator,
          "\n"
        ]
    isTickedOperator = maybe True (Char.isLetter . fst) . T.uncons

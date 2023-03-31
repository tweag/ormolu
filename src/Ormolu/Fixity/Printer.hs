{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Printer for fixity maps.
module Ormolu.Fixity.Printer
  ( printFixityMap,
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

-- | Print out a textual representation of a 'FixityMap'.
printFixityMap :: FixityMap -> Text
printFixityMap =
  TL.toStrict
    . B.toLazyText
    . mconcat
    . fmap renderOne
    . concatMap decompose
    . Map.toList
  where
    decompose :: (OpName, FixityInfo) -> [(FixityDirection, Int, OpName)]
    decompose (operator, FixityInfo {..}) =
      let forDirection dir =
            (dir, fiMinPrecedence, operator)
              : [ (dir, fiMaxPrecedence, operator)
                  | fiMinPrecedence /= fiMaxPrecedence
                ]
       in case fiDirection of
            Nothing -> concatMap forDirection [InfixL, InfixR]
            Just dir -> forDirection dir
    renderOne :: (FixityDirection, Int, OpName) -> Builder
    renderOne (fixityDirection, n, OpName operator) =
      mconcat
        [ case fixityDirection of
            InfixL -> "infixl"
            InfixR -> "infixr"
            InfixN -> "infix",
          " ",
          B.decimal n,
          " ",
          if isTickedOperator operator
            then "`" <> B.fromText operator <> "`"
            else B.fromText operator,
          "\n"
        ]
    isTickedOperator = maybe True (Char.isLetter . fst) . T.uncons

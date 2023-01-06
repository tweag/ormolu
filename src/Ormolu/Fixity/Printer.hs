{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Printer for fixity maps.
module Ormolu.Fixity.Printer
  ( printFixityMap,
  )
where

import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
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

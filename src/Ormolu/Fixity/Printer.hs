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
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Ormolu.Fixity.Internal

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
    decompose :: (String, FixityInfo) -> [(FixityDirection, Int, String)]
    decompose (operator, FixityInfo {..}) =
      let forDirection dir =
            (dir, fiMinPrecedence, operator)
              : [ (dir, fiMaxPrecedence, operator)
                  | fiMinPrecedence /= fiMaxPrecedence
                ]
       in case fiDirection of
            Nothing -> concatMap forDirection [InfixL, InfixR]
            Just dir -> forDirection dir
    renderOne :: (FixityDirection, Int, String) -> Builder
    renderOne (fixityDirection, n, operator) =
      mconcat
        [ case fixityDirection of
            InfixL -> "infixl"
            InfixR -> "infixr"
            InfixN -> "infix",
          " ",
          B.decimal n,
          " ",
          if isTickedOperator operator
            then "`" <> B.fromString operator <> "`"
            else B.fromString operator,
          "\n"
        ]
    isTickedOperator [] = True
    isTickedOperator (x : _) = Char.isLetter x

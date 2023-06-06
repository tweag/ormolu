{-# LANGUAGE OverloadedStrings #-}

-- | Support for CPP.
module Ormolu.Processing.Cpp
  ( cppLines,
    eraseCppLines,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

-- | State of the CPP processor.
data State
  = -- | Outside of CPP directives
    Outside
  | -- | In a conditional expression, with a positive nesting count
    InConditional Int
  | -- | In a continuation (after @\\@), but not in a conditional expression
    InContinuation
  deriving (Eq, Show)

-- | Return an 'IntSet' containing all lines which are affected by CPP.
cppLines :: Text -> IntSet
cppLines input = IntSet.fromAscList $ go Outside (T.lines input `zip` [1 ..])
  where
    go _ [] = []
    go state ((line, i) : ls)
      | any for ["define ", "include ", "undef "] =
          i : go contState ls
      | any for ["ifdef ", "ifndef ", "if "] =
          let state' = case state of
                InConditional nc -> InConditional (nc + 1)
                _ -> InConditional 1
           in i : go state' ls
      | for "endif" =
          let state' = case state of
                InConditional nc | nc > 1 -> InConditional (nc - 1)
                _ -> Outside
           in i : go state' ls
      | otherwise =
          let is = case state of
                Outside -> []
                _ -> [i]
              state' = case state of
                InContinuation -> contState
                _ -> state
           in is <> go state' ls
      where
        for directive = isJust $ do
          s <- T.stripStart <$> T.stripPrefix "#" line
          T.stripPrefix directive s
        contState =
          if "\\" `T.isSuffixOf` line && not inConditional
            then InContinuation
            else Outside
          where
            inConditional = case state of
              InConditional {} -> True
              _ -> False

-- | Replace all lines affected by CPP with blank lines.
eraseCppLines :: Text -> Text
eraseCppLines input =
  T.unlines . fmap eraseCpp $ T.lines input `zip` [1 ..]
  where
    linesToErase = cppLines input
    eraseCpp (x, i) =
      if i `IntSet.member` linesToErase
        then "\n"
        else x

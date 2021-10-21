{-# LANGUAGE OverloadedStrings #-}

-- | Support for CPP.
module Ormolu.Processing.Cpp
  ( cppLines,
  )
where

import Data.Char (isSpace)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as L
import Data.Maybe (isJust)

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
cppLines :: String -> IntSet
cppLines input = IntSet.fromAscList $ go Outside (lines input `zip` [1 ..])
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
          s <- dropWhile isSpace <$> L.stripPrefix "#" line
          L.stripPrefix directive s
        contState =
          if "\\" `L.isSuffixOf` line && not inConditional
            then InContinuation
            else Outside
          where
            inConditional = case state of
              InConditional {} -> True
              _ -> False

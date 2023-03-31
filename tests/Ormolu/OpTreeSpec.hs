{-# LANGUAGE OverloadedStrings #-}

module Ormolu.OpTreeSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Types.Name (mkOccName, varName)
import GHC.Types.Name.Reader (mkRdrUnqual)
import Ormolu.Fixity
import Ormolu.Fixity.Internal (LazyFixityMap (..))
import Ormolu.Printer.Operators
import Test.Hspec

n :: Text -> OpTree Text OpName
n = OpNode

-- | Check that the input tree is actually reassociated as expected.
checkReassociate ::
  -- | Fixity map used for the reassociation
  [(OpName, FixityInfo)] ->
  -- | Input tree
  OpTree Text OpName ->
  -- | Expected output tree
  OpTree Text OpName ->
  Expectation
checkReassociate lFixities inputTree expectedOutputTree =
  removeOpInfo actualOutputTree `shouldBe` expectedOutputTree
  where
    removeOpInfo (OpNode x) = OpNode x
    removeOpInfo (OpBranches exprs ops) =
      OpBranches (removeOpInfo <$> exprs) (opiOp <$> ops)
    actualOutputTree = reassociateOpTree convertName Map.empty fixityMap inputTree
    fixityMap = LazyFixityMap [Map.fromList lFixities]
    convertName = Just . mkRdrUnqual . mkOccName varName . T.unpack . unOpName

-- | Associative list of fixities for operators from "base"
baseFixities :: [(OpName, FixityInfo)]
baseFixities = Map.toList . fromJust $ Map.lookup "base" packageToOps

spec :: Spec
spec = do
  it "flattens a tree correctly" $ do
    let inputTree =
          OpBranches
            [ OpBranches
                [OpBranches [n "a", n "b"] ["+"], n "c"]
                ["+"],
              n "d"
            ]
            ["+"]
        outputTree =
          OpBranches [n "a", n "b", n "c", n "d"] ["+", "+", "+"]
        fixities = [("+", FixityInfo (Just InfixL) 5 5)]
    checkReassociate fixities inputTree outputTree

  it "uses 'minOps' strategy by default" $ do
    let inputTree =
          OpBranches
            [n "a", n "b", n "c", n "d", n "e", n "f"]
            ["*", "*", "+", "*", "-"]
        outputTree =
          OpBranches
            [ OpBranches [n "a", n "b", n "c"] ["*", "*"],
              OpBranches [n "d", n "e"] ["*"],
              n "f"
            ]
            ["+", "-"]
        fixities =
          [ ("+", FixityInfo (Just InfixL) 5 5),
            ("*", FixityInfo (Just InfixL) 7 7),
            ("-", FixityInfo (Just InfixL) 5 5)
          ]
    checkReassociate fixities inputTree outputTree

  it "uses 'maxOps' strategy if 'minOps' strategy fails" $ do
    let inputTree =
          OpBranches
            [n "a", n "b", n "c", n "d", n "e", n "f"]
            ["*", "*", "+", "*", "-"]
        outputTree =
          OpBranches
            [ OpBranches [n "a", n "b", n "c"] ["*", "*"],
              OpBranches [n "d", n "e"] ["*"],
              n "f"
            ]
            ["+", "-"]
        fixities =
          [ ("+", FixityInfo (Just InfixL) 5 7),
            ("*", FixityInfo (Just InfixL) 8 8),
            ("-", FixityInfo (Just InfixL) 4 6)
          ]
    checkReassociate fixities inputTree outputTree

  it
    "defaults to 'hardSplitter' strategy if both 'minOps' and 'maxOps' \
    \strategies fail"
    $ do
      let inputTree =
            OpBranches
              [n "a", n "b", n "c", n "d", n "e", n "f"]
              ["@", "@", "|", "@", "$"]
          outputTree =
            OpBranches
              [ OpBranches
                  [n "a", n "b", n "c", n "d", n "e"]
                  ["@", "@", "|", "@"],
                n "f"
              ]
              ["$"]
          fixities =
            [ ("@", FixityInfo (Just InfixL) 0 5),
              ("|", FixityInfo (Just InfixL) 4 8),
              ("$", FixityInfo (Just InfixR) 0 0)
            ]
      checkReassociate fixities inputTree outputTree

  it "reassociates correctly: complex example 1" $ do
    let inputTree =
          OpBranches
            [n "f", n "1", n "2", n "3", n "4", n "5", n "6"]
            ["$", "+", "*", "$", "*", "+"]
        outputTree =
          OpBranches
            [ n "f",
              OpBranches
                [n "1", OpBranches [n "2", n "3"] ["*"]]
                ["+"],
              OpBranches
                [OpBranches [n "4", n "5"] ["*"], n "6"]
                ["+"]
            ]
            ["$", "$"]
    checkReassociate baseFixities inputTree outputTree

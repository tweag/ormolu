{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.OpTreeSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Types.Name (mkOccName, varName)
import GHC.Types.Name.Reader (mkRdrUnqual)
import Ormolu.Fixity
import Ormolu.Fixity.Internal
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
checkReassociate fixities inputTree expectedOutputTree =
  removeOpInfo actualOutputTree `shouldBe` expectedOutputTree
  where
    removeOpInfo (OpNode x) = OpNode x
    removeOpInfo (OpBranches exprs ops) =
      OpBranches (removeOpInfo <$> exprs) (opiOp <$> ops)
    actualOutputTree = reassociateOpTree convertName modFixityMap inputTree
    modFixityMap = ModuleFixityMap (Map.map Given (Map.fromList fixities))
    convertName = Just . mkRdrUnqual . mkOccName varName . T.unpack . unOpName

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
        fixities = [("+", FixityInfo InfixL 5)]
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
          [ ("+", FixityInfo InfixL 5),
            ("*", FixityInfo InfixL 7),
            ("-", FixityInfo InfixL 5)
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
          [ ("+", FixityInfo InfixL 5),
            ("*", FixityInfo InfixL 8),
            ("-", FixityInfo InfixL 5)
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
            [ ("@", FixityInfo InfixL 4),
              ("|", FixityInfo InfixL 4),
              ("$", FixityInfo InfixR 0)
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
        fixities =
          [ ("$", FixityInfo InfixR 0),
            ("+", FixityInfo InfixL 6),
            ("*", FixityInfo InfixL 7)
          ]
    checkReassociate fixities inputTree outputTree

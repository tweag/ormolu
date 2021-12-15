{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ormolu.OpTreeSpec where

import qualified Data.Map.Strict as Map
import GHC.Types.Name (mkOccName, varName)
import GHC.Types.Name.Reader (mkRdrUnqual)
import Ormolu.Fixity (FixityDirection (..), FixityInfo (FixityInfo))
import Ormolu.Printer.Operators (OpInfo (opiOp), OpTree (..), reassociateOpTree)
import Test.Hspec

type T = OpTree String String

n :: String -> T
n = OpNode

class TreeBuildList ty op t where
  buildTree :: [OpTree ty op] -> [op] -> t

instance TreeBuildList ty op (OpTree ty op) where
  buildTree = OpBranches

instance (TreeBuildList ty op r) => TreeBuildList ty op (op -> OpTree ty op -> r) where
  buildTree exprsAcc opsAcc =
    \op expr -> buildTree (exprsAcc ++ [expr]) (opsAcc ++ [op])

-- | Construct a (sub)tree ('OpBranches') with an easy-to-read syntax.
tree :: (TreeBuildList ty op t) => OpTree ty op -> t
tree expr = buildTree [expr] []

-- | Check that the input tree is actually reassociated as expected.
checkReassociate ::
  -- | Fixity map used for the reassociation
  [(String, FixityInfo)] ->
  -- | Input tree
  OpTree String String ->
  -- | Expected output tree
  OpTree String String ->
  Expectation
checkReassociate lFixities inputTree expectedOutputTree =
  removeOpInfo actualOutputTree `shouldBe` expectedOutputTree
  where
    removeOpInfo (OpNode x) = OpNode x
    removeOpInfo (OpBranches exprs ops) =
      OpBranches (removeOpInfo <$> exprs) (opiOp <$> ops)
    actualOutputTree = reassociateOpTree convertName fixityMap inputTree
    fixityMap = Map.fromList lFixities
    convertName = Just . mkRdrUnqual . mkOccName varName

spec :: Spec
spec = do
  it "flattens a tree correctly" $ do
    let inputTree =
          tree
            ( tree
                ( tree
                    (n "a")
                    "+"
                    (n "b") ::
                    T
                )
                "+"
                (n "c") ::
                T
            )
            "+"
            (n "d")
        outputTree =
          tree (n "a") "+" (n "b") "+" (n "c") "+" (n "d")
        fixities = [("+", FixityInfo (Just InfixL) 5 5)]
    checkReassociate fixities inputTree outputTree

  it "uses 'minOps' strategy by default" $ do
    let inputTree =
          tree
            (n "a")
            "*"
            (n "b")
            "*"
            (n "c")
            "+"
            (n "d")
            "*"
            (n "e")
            "-"
            (n "f")
        outputTree =
          tree
            (tree (n "a") "*" (n "b") "*" (n "c") :: T)
            "+"
            (tree (n "d") "*" (n "e") :: T)
            "-"
            (n "f")
        fixities =
          [ ("+", FixityInfo (Just InfixL) 5 5),
            ("*", FixityInfo (Just InfixL) 7 7),
            ("-", FixityInfo (Just InfixL) 5 5)
          ]
    checkReassociate fixities inputTree outputTree

  it "uses 'maxOps' strategy if 'minOps' strategy fails" $ do
    let inputTree =
          tree
            (n "a")
            "*"
            (n "b")
            "*"
            (n "c")
            "+"
            (n "d")
            "*"
            (n "e")
            "-"
            (n "f")
        outputTree =
          tree
            (tree (n "a") "*" (n "b") "*" (n "c") :: T)
            "+"
            (tree (n "d") "*" (n "e") :: T)
            "-"
            (n "f")
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
            tree
              (n "a")
              "@"
              (n "b")
              "@"
              (n "c")
              "|"
              (n "d")
              "@"
              (n "e")
              "$"
              (n "f")
          outputTree =
            tree
              ( tree
                  (n "a")
                  "@"
                  (n "b")
                  "@"
                  (n "c")
                  "|"
                  (n "d")
                  "@"
                  (n "e") ::
                  T
              )
              "$"
              (n "f")
          fixities =
            [ ("@", FixityInfo (Just InfixL) 0 5),
              ("|", FixityInfo (Just InfixL) 4 8),
              ("$", FixityInfo (Just InfixR) 0 0)
            ]
      checkReassociate fixities inputTree outputTree

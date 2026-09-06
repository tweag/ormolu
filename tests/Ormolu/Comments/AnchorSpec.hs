{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the containment tree and the positional attachment rules.
module Ormolu.Comments.AnchorSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Data.FastString (fsLit)
import GHC.Types.SrcLoc
import Ormolu.Comments.Anchor
import Ormolu.Comments.Tree
import Ormolu.Parser.CommentStream
import Test.Hspec

spec :: Spec
spec = do
  describe "mkSpanForest" $ do
    it "nests spans by containment" $
      mkSpanForest [spn 1 1 9 9, spn 2 1 3 9, spn 2 3 2 8]
        `shouldBe` [ SpanTree
                       (spn 1 1 9 9)
                       [SpanTree (spn 2 1 3 9) [SpanTree (spn 2 3 2 8) []]]
                   ]
    it "keeps siblings in ascending order" $
      fmap stSpan (mkSpanForest [spn 5 1 5 9, spn 1 1 1 9, spn 3 1 3 9])
        `shouldBe` [spn 1 1 1 9, spn 3 1 3 9, spn 5 1 5 9]
    it "treats a repeated span as one element" $
      -- Several AST nodes routinely share a span; the comment can only be
      -- owned once.
      countNodes (mkSpanForest [spn 1 1 9 9, spn 1 1 9 9, spn 1 1 9 9])
        `shouldBe` 1
    it "drops spans that overlap without being contained" $
      countNodes (mkSpanForest [spn 1 1 5 9, spn 3 1 7 9]) `shouldBe` 1
    it "keeps zero-width spans" $
      -- The printer enters a zero-width span at each end of an empty list,
      -- deliberately, so that a comment written inside the brackets has
      -- something to attach to.
      mkSpanForest [spn 1 1 1 1, spn 2 1 2 9]
        `shouldBe` [SpanTree (spn 1 1 1 1) [], SpanTree (spn 2 1 2 9) []]

  describe "anchorFor" $ do
    let forest = mkSpanForest [block, stmt1, stmt2]
        block = spn 1 1 5 10
        stmt1 = spn 2 3 2 9
        stmt2 = spn 4 3 4 9

    it "puts a comment between two elements before the later one" $
      anchorFor forest (ownLine 3 3 3 12) `shouldBe` AnchorBefore stmt2
    it "attaches a comment that trails code to the element it trails" $
      anchorFor forest (trailing 2 12 2 20) `shouldBe` AnchorTrailing stmt1
    it "does not treat a comment as trailing when no code precedes it" $
      -- Same line as the end of stmt1, but alone on its line, so it belongs
      -- to what comes after.
      anchorFor forest (ownLine 2 12 2 20) `shouldBe` AnchorBefore stmt2
    it "attaches a comment after the last element to that element" $
      anchorFor forest (ownLine 5 3 5 9) `shouldBe` AnchorTrailing stmt2
    it "gives a comment inside a childless element to that element" $
      anchorFor (mkSpanForest [spn 1 1 3 3]) (ownLine 2 3 2 9)
        `shouldBe` AnchorInside (spn 1 1 3 3)
    it "leaves a comment outside everything to the module" $
      -- Not trailing the last top-level element: there is nothing it could
      -- trail without being rendered before syntax that preceded it.
      anchorFor forest (ownLine 9 1 9 9) `shouldBe` AnchorModule
    it "leaves a comment to the module when there are no elements at all" $
      anchorFor [] (ownLine 1 1 1 9) `shouldBe` AnchorModule

    -- @f x = -- c@ re-parsed: the comment now falls inside the right-hand
    -- side, which opened on that line and has nothing of its own before the
    -- comment. Without looking one level up it would move onto its own
    -- line, and formatting would not be idempotent.
    it "attaches a comment inside an element that opened on its line to the code before it" $
      let rhs = spn 2 11 3 9
          body = spn 3 3 3 9
       in anchorFor
            (mkSpanForest [block, stmt1, rhs, body])
            (trailing 2 14 2 20)
            `shouldBe` AnchorTrailing stmt1
    it "still lets an element starting on the comment's line lead it" $
      -- The @{-a-}@ of @x = ({-a-} b, c)@ belongs to @b@, not to the @x@
      -- one level up.
      let tuple = spn 2 11 2 30
          b = spn 2 18 2 19
       in anchorFor
            (mkSpanForest [block, stmt1, tuple, b])
            (trailing 2 12 2 17)
            `shouldBe` AnchorBefore b
    it "does not carry a comment out of a list of items" $
      -- @xs ++ [ -- why?@: the comment introduces the items, so carrying it
      -- up to trail @xs@ would drag it out of the brackets.
      let list = spn 2 11 4 9
          itemA = spn 3 3 3 9
          itemB = spn 4 3 4 9
       in anchorFor
            (mkSpanForest [block, stmt1, list, itemA, itemB])
            (trailing 2 14 2 20)
            `shouldBe` AnchorBefore itemA
    it "does not carry a block comment up a level" $
      -- A block comment renders where it stands, so trailing an element one
      -- level up would push it ahead of the tokens that opened the element
      -- it was written inside.
      let rhs = spn 2 11 3 9
          body = spn 3 3 3 9
       in anchorFor
            (mkSpanForest [block, stmt1, rhs, body])
            (blockTrailing 2 14 2 20)
            `shouldBe` AnchorBefore body

    it "does not depend on the order the elements are given in" $
      -- This is the whole point: reordering imports or reassociating an
      -- operator tree must not change who owns a comment.
      anchorFor (mkSpanForest [stmt2, block, stmt1]) (ownLine 3 3 3 12)
        `shouldBe` AnchorBefore stmt2

  describe "attachComments" $
    it "attaches every comment exactly once" $ do
      let comments = [ownLine 3 3 3 12, trailing 4 12 4 20]
          anchors = attachComments comments [spn 1 1 5 10, spn 2 3 2 9, spn 4 3 4 9]
      length anchors `shouldBe` 2
      fmap snd anchors
        `shouldBe` [AnchorBefore (spn 4 3 4 9), AnchorTrailing (spn 4 3 4 9)]

----------------------------------------------------------------------------
-- Helpers

spn :: Int -> Int -> Int -> Int -> RealSrcSpan
spn l1 c1 l2 c2 =
  mkRealSrcSpan
    (mkRealSrcLoc (fsLit "<test>") l1 c1)
    (mkRealSrcLoc (fsLit "<test>") l2 c2)

-- | A comment with code in front of it on the same line.
trailing :: Int -> Int -> Int -> Int -> LComment
trailing l1 c1 l2 c2 = L (spn l1 c1 l2 c2) (Comment True ("-- x" :| []))

-- | A block comment with code in front of it on the same line.
blockTrailing :: Int -> Int -> Int -> Int -> LComment
blockTrailing l1 c1 l2 c2 = L (spn l1 c1 l2 c2) (Comment True ("{- x -}" :| []))

-- | A comment that is alone on its line.
ownLine :: Int -> Int -> Int -> Int -> LComment
ownLine l1 c1 l2 c2 = L (spn l1 c1 l2 c2) (Comment False ("-- x" :| []))

{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.CombinatorsSpec (spec) where

import Ormolu.Anns
import Ormolu.Printer.Combinators
import Ormolu.Printer.Internal
import Test.Hspec
import qualified Data.Text.IO as T

-- NOTE Testing combinators in separation is easy, but it's not very useful
-- either because interesting things happen only when combinators are used
-- together. However, there are too many combinations to test, so we test
-- just a few things here and expect new tests to be added on demand when
-- particularly tricky or problematic use cases are discovered.
--
-- The printing logic is indirectly tested in the main bulk of reformatting
-- tests.

spec :: Spec
spec = do
  stdTest "simple signature" rSimpleSig "ssig"
  stdTest "list" rList0 "list0"
  stdTest "list with annotated name in it" rList1 "list1"
  stdTest "module header" rModuleHeader "mhdr"

-- | Do a standard test prodedure testing both single-line and multi-line
-- layouts.

stdTest
  :: String          -- ^ Test name
  -> R ()            -- ^ Computation to test
  -> FilePath        -- ^ Base file name where expected result is stored
  -> Spec
stdTest name m path = describe name $ do
  let spath = "data/printer/" ++ path ++ "-single.hs"
      mpath = "data/printer/" ++ path ++ "-multi.hs"
  it "single-line works" $
      singleLine m `shouldRender` spath
  it "multi-line works" $
      multiLine m `shouldRender` mpath

----------------------------------------------------------------------------
-- Test computations

rSimpleSig :: R ()
rSimpleSig = line rFn

rList0 :: R ()
rList0 = line . brackets $
  velt
    [ txt "foo"
    , comma >> txt "bar"
    , comma >> txt "baz"
    ]

rList1 :: R ()
rList1 = line . brackets $
  velt
    [ txt "foo"
    , comma >> txt "bar"
    , comma >> rFn
    ]

rFn :: R ()
rFn = velt'
  [ txt "foo"
  , inci $ velt'
    [ do txt ":: "
         parens $ velt'
           [ txt "Int"
           , txt "-> " >> txt "Int"
           ]
    , txt "-> " >> txt "Bool"
    ]
  ]

rModuleHeader :: R ()
rModuleHeader = do
  line $ do
    txt "module "
    txt "MyModule"
  line . inci . parens . velt $
    [ txt "R"
    , comma >> txt "runR"
    , comma >> txt "txt"
    , comma >> txt "blah"
    ]
  line (txt "where")

----------------------------------------------------------------------------
-- Helpers

-- | Run given computation in @'R' ()@ monad and compare result of rendering
-- with contents of a given file.

shouldRender :: R () -> FilePath -> Expectation
shouldRender m path = do
  expected <- T.readFile path
  runR False m mempty mempty emptyAnns `shouldBe` expected

-- | Render using single-line layout.

singleLine :: R () -> R ()
singleLine = enterLayout SingleLine

-- | Render using multi-line layout.

multiLine :: R () -> R ()
multiLine = enterLayout MultiLine

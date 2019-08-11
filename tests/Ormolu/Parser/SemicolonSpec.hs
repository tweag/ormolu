module Ormolu.Parser.SemicolonSpec (spec) where

import Ormolu.Config (defaultConfig)
import Ormolu.Parser
import Ormolu.Parser.Result
import Ormolu.Parser.Semicolon
import Data.Maybe
import Test.Hspec

spec :: Spec
spec = do
  describe "assertSemis" $ do
    stdTest True ["data Foo = Bar"]
    stdTest True ["foo = let i = do i in i"]
    stdTest True ["do { foo }"]
    stdTest True [ "data Foo a where { Bar :: Foo ()"
                 , "                 ; Baz :: Foo Int"
                 , "                 }"
                 ]
    stdTest False ["data Foo a where { Bar :: Foo (); Baz :: Foo Int } "]
    stdTest False ["foo = let i = i; j = i in i"]
    stdTest False ["do { foo; bar }"]



stdTest :: Bool -> [String] -> Spec
stdTest result input = it str $ do
  (_, Right ParseResult { prAnns = anns }) <- parseModule defaultConfig "test" str
  getSemicolonWarning anns `shouldSatisfy` (if result then isNothing else isJust)
 where
  str = mconcat (map (++ "\n") input)

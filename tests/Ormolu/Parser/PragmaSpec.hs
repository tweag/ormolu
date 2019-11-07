module Ormolu.Parser.PragmaSpec (spec) where

import Ormolu.Parser.Pragma
import Test.Hspec

spec :: Spec
spec =
  describe "parsePragma" $ do
    stdTest "{-# LANGUAGE Foo #-}" (Just (PragmaLanguage ["Foo"]))
    stdTest "{-# language Foo #-}" (Just (PragmaLanguage ["Foo"]))
    stdTest "{-#LANGUAGE Foo#-}" (Just (PragmaLanguage ["Foo"]))
    stdTest "{-# LANGUAGE Foo#-}" (Just (PragmaLanguage ["Foo"]))
    stdTest "{-#language Foo#-}" (Just (PragmaLanguage ["Foo"]))
    stdTest "{-# lAngUAGe Foo #-}" (Just (PragmaLanguage ["Foo"]))
    stdTest "{-# LANGUAGE Foo, Bar #-}" (Just (PragmaLanguage ["Foo", "Bar"]))
    stdTest "{-# LANGUAGE Foo Bar #-}" Nothing
    stdTest "{-# BOO Foo #-}" Nothing
    stdTest "something" Nothing
    stdTest "{-# LANGUAGE foo, Bar #-}" Nothing
    stdTest "{-# OPTIONS_GHC foo bar baz  #-}" (Just $ PragmaOptionsGHC "foo bar baz")
    stdTest "{-#OPTIONS_HADDOCK foo, bar, baz  #-}" (Just $ PragmaOptionsHaddock "foo, bar, baz")

stdTest :: String -> Maybe Pragma -> Spec
stdTest input result =
  it input $
    parsePragma input `shouldBe` result

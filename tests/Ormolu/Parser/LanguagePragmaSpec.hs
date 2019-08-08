module Ormolu.Parser.LanguagePragmaSpec (spec) where

import Ormolu.Parser.LanguagePragma
import Test.Hspec
import qualified Data.Set as S

spec :: Spec
spec = do
  describe "parseLanguagePragma" $ do
    stdTest "{-# LANGUAGE Foo #-}" (Just ["Foo"])
    stdTest "{-# language Foo #-}" (Just ["Foo"])
    stdTest "{-#LANGUAGE Foo#-}" (Just ["Foo"])
    stdTest "{-# LANGUAGE Foo#-}" (Just ["Foo"])
    stdTest "{-#language Foo#-}" (Just ["Foo"])
    stdTest "{-# lAngUAGe Foo #-}" (Just ["Foo"])
    stdTest "{-# LANGUAGE Foo, Bar #-}" (Just ["Bar", "Foo"])
    stdTest "{-# LANGUAGE Foo Bar #-}" Nothing
    stdTest "{-# BOO Foo #-}" Nothing
    stdTest "something" Nothing
    stdTest "{-# LANGUAGE foo, Bar #-}" Nothing

stdTest :: String -> Maybe [String] -> Spec
stdTest input result = it input $
  (S.toAscList <$> parseLanguagePragma input) `shouldBe`
    result

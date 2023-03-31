{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Fixity.ParserSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  describe "parseFixtiyDeclaration" $ do
    it "parses a simple infixr declaration" $
      parseFixityDeclaration "infixr 5 $"
        `shouldParse` [("$", FixityInfo (Just InfixR) 5 5)]
    it "parses a simple infixl declaration" $
      parseFixityDeclaration "infixl 5 $"
        `shouldParse` [("$", FixityInfo (Just InfixL) 5 5)]
    it "parses a simple infix declaration" $
      parseFixityDeclaration "infix 5 $"
        `shouldParse` [("$", FixityInfo (Just InfixN) 5 5)]
    it "parses a declaration for a ticked identifier" $
      parseFixityDeclaration "infixl 5 `foo`"
        `shouldParse` [("foo", FixityInfo (Just InfixL) 5 5)]
    it "parses a declaration for a ticked identifier (constructor case)" $
      parseFixityDeclaration "infixl 5 `Foo`"
        `shouldParse` [("Foo", FixityInfo (Just InfixL) 5 5)]
    it "parses a multi-operator declaration" $
      parseFixityDeclaration "infixl 5 $, ., `Foo`, `bar`"
        `shouldParse` [ ("$", FixityInfo (Just InfixL) 5 5),
                        (".", FixityInfo (Just InfixL) 5 5),
                        ("Foo", FixityInfo (Just InfixL) 5 5),
                        ("bar", FixityInfo (Just InfixL) 5 5)
                      ]
    it "parses a declaration with a unicode operator" $
      parseFixityDeclaration "infixr 5 ×"
        `shouldParse` [("×", FixityInfo (Just InfixR) 5 5)]
    it "fails with correct parse error (keyword wrong)" $
      parseFixityDeclaration "foobar 5 $"
        `shouldFailWith` err
          0
          ( mconcat
              [ utoks "foobar",
                etoks "infix",
                etoks "infixl",
                etoks "infixr"
              ]
          )
    it "fails with correct parse error (missing operator)" $
      parseFixityDeclaration "infixr 5 "
        `shouldFailWith` err
          9
          ( mconcat
              [ ueof,
                etok '`',
                elabel "operator character"
              ]
          )
    it "fails with correct parse error (trailing comma)" $
      parseFixityDeclaration "infixr 5 ., "
        `shouldFailWith` err
          12
          ( mconcat
              [ ueof,
                etok '`',
                elabel "operator character"
              ]
          )
  describe "parseFixityMap" $ do
    it "parses the empty input without choking" $
      parseFixityMap "" ""
        `shouldParse` Map.empty
    it "parses a collection of declarations" $
      -- The example is taken from base.
      parseFixityMap
        ""
        ( T.unlines
            [ "infixr 9  .",
              "infixr 5  ++",
              "infixl 4  <$",
              "infixl 1  >>, >>=",
              "infixr 1  =<<",
              "infixr 0  $, $!",
              "infixl 4 <*>, <*, *>, <**>"
            ]
        )
        `shouldParse` Map.fromList
          [ ("$", FixityInfo (Just InfixR) 0 0),
            ("$!", FixityInfo (Just InfixR) 0 0),
            ("*>", FixityInfo (Just InfixL) 4 4),
            ("++", FixityInfo (Just InfixR) 5 5),
            (".", FixityInfo (Just InfixR) 9 9),
            ("<$", FixityInfo (Just InfixL) 4 4),
            ("<*", FixityInfo (Just InfixL) 4 4),
            ("<**>", FixityInfo (Just InfixL) 4 4),
            ("<*>", FixityInfo (Just InfixL) 4 4),
            ("=<<", FixityInfo (Just InfixR) 1 1),
            (">>", FixityInfo (Just InfixL) 1 1),
            (">>=", FixityInfo (Just InfixL) 1 1)
          ]
    it "combines conflicting declarations correctly" $
      parseFixityMap
        ""
        ( T.unlines
            [ "infixr 9 ., ^",
              "infixr 7 ., $",
              "infixr 9 ^ ",
              "infixl 7 $"
            ]
        )
        `shouldParse` Map.fromList
          [ ("$", FixityInfo Nothing 7 7),
            (".", FixityInfo (Just InfixR) 7 9),
            ("^", FixityInfo (Just InfixR) 9 9)
          ]
    it "handles CRLF line endings correctly" $
      parseFixityMap ""
        `shouldSucceedOn` ( unlinesCrlf
                              [ "infixr 9  .",
                                "infixr 5  ++"
                              ]
                          )
    it "fails with correct parse error (keyword wrong second line)" $
      parseFixityMap "" "infixr 5 .\nfoobar 5 $"
        `shouldFailWith` err
          11
          ( mconcat
              [ utok 'f',
                etoks "infix",
                etoks "infixl",
                etoks "infixr",
                eeof
              ]
          )

unlinesCrlf :: [Text] -> Text
unlinesCrlf = T.concat . fmap (<> "\r\n")

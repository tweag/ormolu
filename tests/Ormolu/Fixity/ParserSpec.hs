{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Fixity.ParserSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Ormolu.Fixity
import Ormolu.Fixity.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec.Error (ErrorFancy (..))

spec :: Spec
spec = do
  describe "parseFixtiyDeclaration" $ do
    it "parses a simple infixr declaration" $
      parseFixityDeclaration "infixr 5 $"
        `shouldParse` [("$", FixityInfo InfixR 5)]
    it "parses a simple infixl declaration" $
      parseFixityDeclaration "infixl 5 $"
        `shouldParse` [("$", FixityInfo InfixL 5)]
    it "parses a simple infix declaration" $
      parseFixityDeclaration "infix 5 $"
        `shouldParse` [("$", FixityInfo InfixN 5)]
    it "parses a declaration for a ticked identifier" $
      parseFixityDeclaration "infixl 5 `foo`"
        `shouldParse` [("foo", FixityInfo InfixL 5)]
    it "parses a declaration for a ticked identifier (constructor case)" $
      parseFixityDeclaration "infixl 5 `Foo`"
        `shouldParse` [("Foo", FixityInfo InfixL 5)]
    it "parses a multi-operator declaration" $
      parseFixityDeclaration "infixl 5 $, ., `Foo`, `bar`"
        `shouldParse` [ ("$", FixityInfo InfixL 5),
                        (".", FixityInfo InfixL 5),
                        ("Foo", FixityInfo InfixL 5),
                        ("bar", FixityInfo InfixL 5)
                      ]
    it "parses a declaration with a unicode operator" $
      parseFixityDeclaration "infixr 5 ×"
        `shouldParse` [("×", FixityInfo InfixR 5)]
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
    it "fails with correct parse error (precedence greater than 9)" $
      parseFixityDeclaration "infixl 10 $"
        `shouldFailWith` errFancy
          7
          (fancy (ErrorFail "precedence should not be greater than 9"))
  describe "parseFixityOverrides" $ do
    it "parses the empty input without choking" $
      parseFixityOverrides "" ""
        `shouldParse` FixityOverrides Map.empty
    it "parses a collection of declarations" $
      -- The example is taken from base.
      parseFixityOverrides
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
        `shouldParse` FixityOverrides
          ( Map.fromList
              [ ("$", FixityInfo InfixR 0),
                ("$!", FixityInfo InfixR 0),
                ("*>", FixityInfo InfixL 4),
                ("++", FixityInfo InfixR 5),
                (".", FixityInfo InfixR 9),
                ("<$", FixityInfo InfixL 4),
                ("<*", FixityInfo InfixL 4),
                ("<**>", FixityInfo InfixL 4),
                ("<*>", FixityInfo InfixL 4),
                ("=<<", FixityInfo InfixR 1),
                (">>", FixityInfo InfixL 1),
                (">>=", FixityInfo InfixL 1)
              ]
          )
    it "combines conflicting declarations correctly" $
      parseFixityOverrides
        ""
        ( T.unlines
            [ "infixr 9 ., ^",
              "infixr 7 ., $",
              "infixr 9 ^ ",
              "infixl 7 $"
            ]
        )
        `shouldParse` FixityOverrides
          ( Map.fromList
              [ ("$", FixityInfo InfixL 7),
                (".", FixityInfo InfixR 7),
                ("^", FixityInfo InfixR 9)
              ]
          )
    it "handles CRLF line endings correctly" $
      parseFixityOverrides ""
        `shouldSucceedOn` unlinesCrlf
          [ "infixr 9  .",
            "infixr 5  ++"
          ]
    it "fails with correct parse error (keyword wrong second line)" $
      parseFixityOverrides "" "infixr 5 .\nfoobar 5 $"
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

{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Fixity.ParserSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
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
  describe "parseDotOrmolu" $ do
    it "parses the empty input without choking" $
      parseDotOrmolu "" ""
        `shouldParse` (FixityOverrides Map.empty, ModuleReexports Map.empty)
    it "parses a collection of fixity declarations" $
      -- The example is taken from base.
      parseDotOrmolu
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
        `shouldParse` ( exampleFixityOverrides,
                        ModuleReexports Map.empty
                      )
    it "combines conflicting fixity declarations correctly" $
      parseDotOrmolu
        ""
        ( T.unlines
            [ "infixr 9 ., ^",
              "infixr 7 ., $",
              "infixr 9 ^ ",
              "infixl 7 $"
            ]
        )
        `shouldParse` ( FixityOverrides
                          ( Map.fromList
                              [ ("$", FixityInfo InfixL 7),
                                (".", FixityInfo InfixR 7),
                                ("^", FixityInfo InfixR 9)
                              ]
                          ),
                        ModuleReexports Map.empty
                      )
    it "handles CRLF line endings correctly" $
      parseDotOrmolu ""
        `shouldSucceedOn` unlinesCrlf
          [ "infixr 9  .",
            "infixr 5  ++"
          ]
    it "fails with correct parse error (keyword wrong second line)" $
      parseDotOrmolu "" "infixr 5 .\nfoobar 5 $"
        `shouldFailWith` err
          11
          ( mconcat
              [ utok 'f',
                etoks "infix",
                etoks "infixl",
                etoks "infixr",
                etoks "module",
                eeof
              ]
          )
    it "parses module re-exports and combines them correctly" $
      parseDotOrmolu
        ""
        ( T.unlines
            [ "module Control.Lens exports Control.Lens.Lens",
              "module Control.Lens exports Control.Lens.At",
              "module Text.Megaparsec exports Control.Monad.Combinators"
            ]
        )
        `shouldParse` (FixityOverrides Map.empty, exampleModuleReexports)
    it "parses fixity declarations + module re-export declarations with blanks" $
      parseDotOrmolu
        ""
        ( T.unlines
            [ "module Control.Lens exports Control.Lens.Lens",
              "",
              "infixr 5  ++",
              "infixl 4  <$",
              "",
              "",
              "module Control.Lens exports Control.Lens.At",
              "infixr 9  .",
              "module Text.Megaparsec exports Control.Monad.Combinators",
              "infixl 1  >>, >>=",
              "infixr 1  =<<",
              "",
              "infixr 0  $, $!",
              "infixl 4 <*>, <*, *>, <**>"
            ]
        )
        `shouldParse` (exampleFixityOverrides, exampleModuleReexports)
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
  describe "parseModuleReexportDeclaration" $ do
    it "parses a re-export declaration" $
      parseModuleReexportDeclaration "module Control.Lens exports Control.Lens.Lens"
        `shouldParse` ( "Control.Lens",
                        "Control.Lens.Lens" :| []
                      )
    it "fails with correct parse error (keyword wrong)" $
      parseModuleReexportDeclaration "foo Control.Lens exports Control.Lens.Lens"
        `shouldFailWith` err
          0
          ( mconcat
              [ utoks "foo Co",
                etoks "module"
              ]
          )
    it "fails with correct parse error (module syntax)" $
      parseModuleReexportDeclaration "module control.Lens exports Control.Lens.Lens"
        `shouldFailWith` err
          7
          ( mconcat
              [ utok 'c',
                elabel "module name"
              ]
          )
    it "fails with correct parse error (typo: export intead exports)" $
      parseModuleReexportDeclaration "module Control.Lens export Control.Lens.Lens"
        `shouldFailWith` err
          20
          ( mconcat
              [ utoks "export ",
                etoks "exports"
              ]
          )

exampleFixityOverrides :: FixityOverrides
exampleFixityOverrides =
  FixityOverrides
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

exampleModuleReexports :: ModuleReexports
exampleModuleReexports =
  ModuleReexports . Map.fromList $
    [ ( "Control.Lens",
        "Control.Lens.At" :| ["Control.Lens.Lens"]
      ),
      ( "Text.Megaparsec",
        "Control.Monad.Combinators" :| []
      )
    ]

unlinesCrlf :: [Text] -> Text
unlinesCrlf = T.concat . fmap (<> "\r\n")

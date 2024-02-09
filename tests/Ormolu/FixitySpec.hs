{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ormolu.FixitySpec (spec) where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName (PackageName)
import GHC.Types.Name (OccName)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader
import Language.Haskell.Syntax.ImpExp (ImportListInterpretation (..))
import Language.Haskell.Syntax.Module.Name (mkModuleName)
import Ormolu.Fixity
import Ormolu.Fixity.Imports
import Ormolu.Fixity.Internal
import Ormolu.Utils (showOutputable)
import Test.Hspec

instance Show RdrName where
  show = showOutputable

spec :: Spec
spec = do
  it "gives the correct fixity info for (:) (built-in)" $
    checkFixities
      []
      []
      [(unqual ":", FixityApproximation (Just InfixR) 5 5)]
  it "does not know operators from base if base is not a dependency" $
    checkFixities
      []
      []
      [ (unqual "$", defaultFixityApproximation),
        (unqual "+", defaultFixityApproximation),
        (unqual "++", defaultFixityApproximation)
      ]
  it "does not know operators from base if Prelude is not imported" $
    checkFixities
      []
      []
      [ (unqual "$", defaultFixityApproximation),
        (unqual "+", defaultFixityApproximation),
        (unqual "++", defaultFixityApproximation)
      ]
  it "infers fixities of operators from base correctly" $
    checkFixities
      ["base"]
      [import_ "Prelude"]
      [ (unqual "$", FixityApproximation (Just InfixR) 0 0),
        (unqual "+", FixityApproximation (Just InfixL) 6 6),
        (unqual "++", FixityApproximation (Just InfixR) 5 5)
      ]
  it "does not know (>>>) when Control.Category is not imported" $
    checkFixities
      ["base"]
      [import_ "Prelude"]
      [ (unqual ">>>", defaultFixityApproximation)
      ]
  it "infer correct fixity for (>>>) when Control.Category is imported" $
    checkFixities
      ["base"]
      [ import_ "Prelude",
        import_ "Control.Category"
      ]
      [ (unqual ">>>", FixityApproximation (Just InfixR) 1 1)
      ]
  it "handles 'as' imports correctly" $
    checkFixities
      ["base"]
      [ import_ "Control.Category" & as_ "Foo"
      ]
      [ (unqual ">>>", FixityApproximation (Just InfixR) 1 1),
        (qual "Foo" ">>>", FixityApproximation (Just InfixR) 1 1),
        (qual "Bar" ">>>", defaultFixityApproximation)
      ]
  it "handles 'qualified' imports correctly" $
    checkFixities
      ["base"]
      [import_ "Control.Category" & qualified_]
      [ (unqual ">>>", defaultFixityApproximation),
        (qual "Control.Category" ">>>", FixityApproximation (Just InfixR) 1 1)
      ]
  it "handles 'qualified as' imports correctly" $
    checkFixities
      ["base"]
      [import_ "Control.Category" & qualified_ & as_ "Foo"]
      [ (unqual ">>>", defaultFixityApproximation),
        (qual "Control.Category" ">>>", defaultFixityApproximation),
        (qual "Foo" ">>>", FixityApproximation (Just InfixR) 1 1)
      ]
  it "handles explicit import lists correctly" $
    checkFixities
      ["base"]
      [import_ "Prelude" & exactly_ ["$"]]
      [ (unqual "$", FixityApproximation (Just InfixR) 0 0),
        (unqual "+", defaultFixityApproximation)
      ]
  it "handles hiding import lists correctly" $
    checkFixities
      ["base"]
      [import_ "Prelude" & hiding_ ["$"]]
      [ (unqual "$", defaultFixityApproximation),
        (unqual "+", FixityApproximation (Just InfixL) 6 6),
        (unqual "++", FixityApproximation (Just InfixR) 5 5)
      ]
  it "handles qualified imports with explicit import lists correctly" $
    checkFixities
      ["base"]
      [import_ "Prelude" & qualified_ & exactly_ ["$"]]
      [ (unqual "$", defaultFixityApproximation),
        (qual "Prelude" "$", FixityApproximation (Just InfixR) 0 0),
        (unqual "+", defaultFixityApproximation),
        (qual "Prelude" "+", defaultFixityApproximation)
      ]
  it "handles qualified import with hiding correctly" $
    checkFixities
      ["base"]
      [import_ "Prelude" & qualified_ & hiding_ ["$"]]
      [ (unqual "$", defaultFixityApproximation),
        (qual "Prelude" "$", defaultFixityApproximation),
        (unqual "+", defaultFixityApproximation),
        (qual "Prelude" "+", FixityApproximation (Just InfixL) 6 6)
      ]
  it "handles qualified import and explicit import lists correctly (1)" $
    checkFixities
      ["base"]
      [ import_ "Prelude" & qualified_,
        import_ "Prelude" & exactly_ ["$"]
      ]
      [ (unqual "$", FixityApproximation (Just InfixR) 0 0),
        (qual "Prelude" "$", FixityApproximation (Just InfixR) 0 0),
        (unqual "+", defaultFixityApproximation),
        (qual "Prelude" "+", FixityApproximation (Just InfixL) 6 6)
      ]
  it "handles qualified import and explicit import lists correctly (2)" $
    checkFixities
      ["base"]
      [ import_ "Prelude" & exactly_ ["$"],
        import_ "Prelude" & qualified_
      ]
      [ (unqual "$", FixityApproximation (Just InfixR) 0 0),
        (qual "Prelude" "$", FixityApproximation (Just InfixR) 0 0),
        (unqual "+", defaultFixityApproximation),
        (qual "Prelude" "+", FixityApproximation (Just InfixL) 6 6)
      ]
  it "handles qualified import and hiding import correctly (1)" $
    checkFixities
      ["base"]
      [ import_ "Prelude" & qualified_,
        import_ "Prelude" & hiding_ ["$"]
      ]
      [ (unqual "$", defaultFixityApproximation),
        (qual "Prelude" "$", FixityApproximation (Just InfixR) 0 0),
        (unqual "+", FixityApproximation (Just InfixL) 6 6),
        (qual "Prelude" "+", FixityApproximation (Just InfixL) 6 6)
      ]
  it "handles qualified import and hiding import correctly (2)" $
    checkFixities
      ["base"]
      [ import_ "Prelude" & hiding_ ["$"],
        import_ "Prelude" & qualified_
      ]
      [ (unqual "$", defaultFixityApproximation),
        (qual "Prelude" "$", FixityApproximation (Just InfixR) 0 0),
        (unqual "+", FixityApproximation (Just InfixL) 6 6),
        (qual "Prelude" "+", FixityApproximation (Just InfixL) 6 6)
      ]
  it "works for several imports from different packages" $
    checkFixities
      ["base", "esqueleto"]
      [ import_ "Prelude",
        import_ "Database.Esqueleto.Experimental" & qualified_ & as_ "E"
      ]
      [ (unqual "$", FixityApproximation (Just InfixR) 0 0),
        (qual "E" "++.", FixityApproximation (Just InfixR) 5 5),
        (qual "E" "on", FixityApproximation (Just InfixN) 9 9)
      ]
  it "merges approximations in case of a conflict" $
    checkFixities
      ["fclabels", "persistent"]
      [ import_ "Data.Label.Monadic",
        import_ "Database.Persist"
      ]
      [ (unqual "=.", FixityApproximation (Just InfixR) 2 3)
      ]
  it "correctly handles package-qualified imports (1)" $
    checkFixities
      ["esqueleto"]
      [package_ "esqueleto" $ import_ "Database.Esqueleto.Experimental"]
      [(unqual "++.", FixityApproximation (Just InfixR) 5 5)]
  it "correctly handles package-qualified imports (2)" $
    checkFixities
      ["esqueleto"]
      [package_ "bob" $ import_ "Database.Esqueleto.Experimental"]
      [(unqual "++.", defaultFixityApproximation)]
  it "default module re-exports: Control.Lens brings into scope Control.Lens.Lens" $
    checkFixities
      ["lens"]
      ( applyModuleReexports
          defaultModuleReexports
          [import_ "Control.Lens"]
      )
      [(unqual "<+~", FixityApproximation (Just InfixR) 4 4)]
  it "default module re-exports: Control.Lens qualified brings into scope Control.Lens.Lens" $
    checkFixities
      ["lens"]
      ( applyModuleReexports
          defaultModuleReexports
          [import_ "Control.Lens" & qualified_]
      )
      [ (unqual "<+~", defaultFixityApproximation),
        (qual "Control.Lens.Lens" "<+~", defaultFixityApproximation),
        (qual "Control.Lens" "<+~", FixityApproximation (Just InfixR) 4 4)
      ]
  it "default module re-exports: Control.Lens qualified as brings into scope Control.Lens.Lens" $
    checkFixities
      ["lens"]
      ( applyModuleReexports
          defaultModuleReexports
          [import_ "Control.Lens" & qualified_ & as_ "L"]
      )
      [ (unqual "<+~", defaultFixityApproximation),
        (qual "Control.Lens.Lens" "<+~", defaultFixityApproximation),
        (qual "Control.Lens" "<+~", defaultFixityApproximation),
        (qual "L" "<+~", FixityApproximation (Just InfixR) 4 4)
      ]
  it "re-export chains: exported module can itself re-export another module" $ do
    let reexports =
          ModuleReexports $
            Map.insert
              "Foo"
              ((Nothing, "Control.Lens") :| [])
              (unModuleReexports defaultModuleReexports)
    checkFixities
      ["lens"]
      ( applyModuleReexports
          reexports
          [import_ "Foo"]
      )
      [ (unqual "<+~", FixityApproximation (Just InfixR) 4 4)
      ]

-- | Build a fixity map using the Hoogle database and then check the fixity
-- of the specified subset of operators.
checkFixities ::
  -- | List of dependencies
  [PackageName] ->
  -- | Imports
  [FixityImport] ->
  -- | Associative list representing a subset of the resulting fixity map
  -- that should be checked.
  [(RdrName, FixityApproximation)] ->
  Expectation
checkFixities dependencies fixityImports expectedResult =
  actualResult `shouldBe` expectedResult
  where
    actualResult =
      fmap
        (\(k, _) -> (k, inferFixity False k resultMap))
        expectedResult
    resultMap =
      moduleFixityMap
        (packageFixityMap (Set.fromList dependencies))
        fixityImports

qual :: String -> OpName -> RdrName
qual moduleName opName = mkRdrQual (mkModuleName moduleName) (opNameToOccName opName)

unqual :: OpName -> RdrName
unqual = mkRdrUnqual . opNameToOccName

opNameToOccName :: OpName -> OccName
opNameToOccName = mkVarOcc . T.unpack . unOpName

-- | Explicitly specify the package.
package_ :: PackageName -> FixityImport -> FixityImport
package_ packageName fixityImport =
  fixityImport
    { fimportPackage = Just packageName
    }

-- | Construct a simple 'FixityImport'.
import_ :: ModuleName -> FixityImport
import_ moduleName =
  FixityImport
    { fimportPackage = Nothing,
      fimportModule = moduleName,
      fimportQualified = UnqualifiedAndQualified moduleName,
      fimportList = Nothing
    }

-- | Adds an alias for an import.
as_ :: ModuleName -> FixityImport -> FixityImport
as_ moduleName fixityImport =
  fixityImport
    { fimportQualified = case fimportQualified fixityImport of
        UnqualifiedAndQualified _ -> UnqualifiedAndQualified moduleName
        OnlyQualified _ -> OnlyQualified moduleName
    }

-- | Qualified imports.
qualified_ :: FixityImport -> FixityImport
qualified_ fixityImport =
  fixityImport
    { fimportQualified = case fimportQualified fixityImport of
        UnqualifiedAndQualified m -> OnlyQualified m
        OnlyQualified m -> OnlyQualified m
    }

-- | Exact import lists.
exactly_ :: [OpName] -> FixityImport -> FixityImport
exactly_ opNames fixityImports =
  fixityImports
    { fimportList = Just (Exactly, opNames)
    }

-- | Hiding.
hiding_ :: [OpName] -> FixityImport -> FixityImport
hiding_ opNames fixityImports =
  fixityImports
    { fimportList = Just (EverythingBut, opNames)
    }

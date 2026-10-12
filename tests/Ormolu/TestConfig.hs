{-# LANGUAGE OverloadedStrings #-}

-- | The 'Config' that all corpora of test inputs are formatted with.
--
-- Fixity information affects the shape of operator trees, and therefore
-- both the layout and which AST element claims a comment, so every spec
-- that formats an input file has to agree on it.
module Ormolu.TestConfig
  ( exampleConfig,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Ormolu
import Ormolu.Fixity

-- | The configuration to use for a test input at the given path.
exampleConfig :: FilePath -> Config RegionIndices
exampleConfig inputPath =
  defaultConfig
    { cfgSourceType = detectSourceType inputPath,
      cfgFixityOverrides = testsuiteOverrides,
      cfgDependencies =
        Set.fromList
          [ "base",
            "esqueleto",
            "hspec",
            "lens",
            "megaparsec",
            "optics",
            "relude",
            "rio",
            "servant"
          ]
    }

-- | Fixity overrides that are to be used with the test inputs.
testsuiteOverrides :: FixityOverrides
testsuiteOverrides =
  FixityOverrides
    ( Map.fromList
        [ (".=", FixityInfo InfixR 8),
          ("#", FixityInfo InfixR 5),
          (">~<", FixityInfo InfixR 3),
          ("|~|", FixityInfo InfixR 3.3),
          ("<~>", FixityInfo InfixR 3.7)
        ]
    )

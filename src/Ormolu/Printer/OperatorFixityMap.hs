module Ormolu.Printer.OperatorFixityMap (fixityMap) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Types.Basic ( FixityDirection (..) )

import Ormolu.Printer.FixityInfo
  ( FixityInfo (..),
  )

fixityMap :: Map String FixityInfo
fixityMap =
  Map.fromList [
    (
      "!!!",
      -- infixl 9 in 
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!!-",
      -- infixl 9 in 
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!!/",
      -- infixl 7 in linear, linear-accelerate, GPipe-Core
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=7, fixMaxPrec=7}
    ),
    (
      "!!>",
      -- infixr 1 in zeolite-lang
      FixityInfo {fixDirection=Just InfixR, fixMinPrec=1, fixMaxPrec=1}
    ),
    (
      "!!@#@$",
      -- infixl 9 in singletons-base
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!!@#@$$",
      -- infixl 9 in singletons-base
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!!@#@$$$",
      -- infixl 9 in singletons-base
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!#!",
      -- infixr 2 in rings
      FixityInfo {fixDirection=Just InfixR, fixMinPrec=2, fixMaxPrec=2}
    ),
    (
      "!$$",
      -- infixl 9 in regex
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!$$?",
      -- infixl 9 in 
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!",
      -- infixl 9 in lmdb-simple, unordered-containers, hmatrix, hakaru, vector, hashmap-io, haskell2020, vector-text, array, IntervalMap, cobot, strict-containers, containers, step-function, pringletons, squeal-postgresql, named, dynamic, rio, comfort-array, total-map, hTensor, monoidal-containers, minilight, yarn-lock, unpacked-containers, base, BNFC-meta, multi-containers, rethinkdb, data-category, cobot-io, nonempty-containers, planet-mitchell, postgresql-tx-squeal, selda, fused-effects-squeal, unordered-intmap, sdp, nuha, calamity, raw-feldspar, accelerate, dependent-map, HMap, data-interval
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
      -- infixl 8 in relational-record, relational-schemas, nice-html, rowdy-yesod, relational-query, relational-record-examples
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=8, fixMaxPrec=8}
      -- infixl 2 in xml-isogen
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=2, fixMaxPrec=2}
      -- infix 9 in monadiccp
        <> FixityInfo {fixDirection=Just InfixN, fixMinPrec=9, fixMaxPrec=9}
      -- infixr 2 in morphisms
        <> FixityInfo {fixDirection=Just InfixR, fixMinPrec=2, fixMaxPrec=2}
      -- infixr 9 in clay
        <> FixityInfo {fixDirection=Just InfixR, fixMinPrec=9, fixMaxPrec=9}
      -- infixl 6 in language-css
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=6, fixMaxPrec=6}
      -- infixl 1 in propellor
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=1, fixMaxPrec=1}
      -- infixl 4 in massiv
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=4, fixMaxPrec=4}
      -- infixr 0 in pandora
        <> FixityInfo {fixDirection=Just InfixR, fixMinPrec=0, fixMaxPrec=0}
    ),
    (
      "!!",
      -- infixl 9 in fay, singletons-base, heart-core, shakers, kerry, bitstream, vector, papa-lens-implement, gogol-core, haskell2020, vector-text, haxl, rebase, relude, Agda, base-compat, constrained-categories, numhask, rstream, base-prelude, tlex-core, ghc, mit-3qvpPyAi6mH, ghc-lib-parser, rio, hasktorch-codegen, biohazard, hedgehog, haskell-ci, xmonad-contrib, supermonad, hvect, preludeplus, preamble, fay-base, base, darcs, Cabal, sprinkles, numeric-prelude, dimensional, distribution-opensuse, protolude, hsdev, planet-mitchell, base-compat-batteries, LambdaHack, nuha, spiros, dino, accelerate, hledger-web, llvm-hs-pure, precursor, universum, ralist, yesod-paginator, lol, b9, prologue, faktory
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
      -- infixl 8 in indigo
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=8, fixMaxPrec=8}
    ),
    (
      "!!*",
      -- infixl 7 in linear, linear-accelerate, GPipe-Core
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=7, fixMaxPrec=7}
      -- infixl 9 in 
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!!.",
      -- infixl 8 in yarn2nix
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=8, fixMaxPrec=8}
      -- infixl 9 in 
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!!?",
      -- infix 9 in chiasma, cornea, relude, polysemy-resume, ribosome
      FixityInfo {fixDirection=Just InfixN, fixMinPrec=9, fixMaxPrec=9}
      -- infixl 9 in 
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!#",
      -- infixr 2 in rings
      FixityInfo {fixDirection=Just InfixR, fixMinPrec=2, fixMaxPrec=2}
      -- infixl 9 in 
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!#>",
      -- infixr 8 in qchas, hmatrix
      FixityInfo {fixDirection=Just InfixR, fixMinPrec=8, fixMaxPrec=8}
      -- infixl 5 in sdp
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=5, fixMaxPrec=5}
    ),
    (
      "!$",
      -- infixl 9 in regex
      FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
      -- infixr 0 in hlatex
        <> FixityInfo {fixDirection=Just InfixR, fixMinPrec=0, fixMaxPrec=0}
      -- infixl 0 in cryptol
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=0, fixMaxPrec=0}
    ),
    (
      "!$!",
      -- infixr 5 in functor-combinators
      FixityInfo {fixDirection=Just InfixR, fixMinPrec=5, fixMaxPrec=5}
      -- infixl 9 in 
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    ),
    (
      "!$?",
      -- infixr 0 in hlatex
      FixityInfo {fixDirection=Just InfixR, fixMinPrec=0, fixMaxPrec=0}
      -- infixl 9 in 
        <> FixityInfo {fixDirection=Just InfixL, fixMinPrec=9, fixMaxPrec=9}
    )
  ]

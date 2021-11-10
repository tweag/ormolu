#!/usr/bin/env nix-shell
#!nix-shell default.nix -A dev.cabalAndOrmolu -i bash --pure
#
# Format Ormolu using current version of Ormolu.

set -e

export LANG="C.UTF-8"

cabal format

ormolu -e -m inplace $(find app -type f -name "*.hs")
ormolu -e -m inplace $(find src -type f -name "*.hs-boot" -o -name "*.hs")
ormolu -e -m inplace $(find tests -type f -name "*.hs")
ormolu -e -m inplace $(find ormolu-live/src -type f -name "*.hs")
ormolu -e -m inplace $(find extract-hoogle-hackage-info/src -type f -name "*.hs")
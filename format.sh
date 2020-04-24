#!/usr/bin/env nix-shell
#!nix-shell default.nix -A dev.withOrmolu -i bash --pure
#
# Format Ormolu using current version of Ormolu.

set -e

export LANG="C.UTF-8"

cabal format

ormolu -m inplace $(find app -type f -name "*.hs")
ormolu -m inplace $(find src -type f \( -name "*.hs" -o -name "*.hs-boot" \))
ormolu -m inplace $(find tests -type f -name "*.hs")

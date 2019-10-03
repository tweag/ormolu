#!/usr/bin/env nix-shell
#!nix-shell -p "(import ./default.nix {}).ormolu" -i bash --pure
#
# Format Ormolu using current version of Ormolu.

set -e

export LANG="C.UTF-8"

ormolu --mode inplace $(find src -type f \( -name "*.hs" -o -name "*.hs-boot" \))
ormolu --mode inplace $(find tests -type f -name "*.hs")

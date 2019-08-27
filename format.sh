#!/usr/bin/env bash
#
# Format Ormolu using current version of Ormolu.

set -e

export LANG="en_US.UTF-8"

nix run -f default.nix ormolu locales -c ormolu --mode inplace $(find src -type f \( -name "*.hs" -o -name "*.hs-boot" \))
nix run -f default.nix ormolu locales -c ormolu --mode inplace $(find tests -type f -name "*.hs")

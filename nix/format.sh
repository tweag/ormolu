# Format Ormolu using current version of Ormolu.

export LANG="C.UTF-8"

cabal format
(cd extract-hackage-info && cabal format)

export dirs="src app tests extract-hackage-info/src"
# shellcheck disable=SC2046,SC2086
ormolu -m inplace $(find $dirs -type f -name "*.hs" -o -name "*.hs-boot")

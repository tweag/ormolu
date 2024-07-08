# Format Ormolu using current version of Ormolu.

export LANG="C.UTF-8"

export dirs="src app tests extract-hackage-info/src ormolu-live/app"
# shellcheck disable=SC2046,SC2086
ormolu -m inplace $(find $dirs -type f -name "*.hs" -o -name "*.hs-boot")

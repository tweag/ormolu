set -e

cabal update
cabal build exe:ormolu --enable-executable-static --ghc-options="-split-sections"

cp $(find dist-newstyle -name ormolu -type f) .
strip -s ormolu

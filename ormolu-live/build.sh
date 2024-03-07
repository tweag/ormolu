#!/usr/bin/env bash
set -e
WDIR="$(mktemp -d)"
trap 'rm -rf -- "$WDIR"' EXIT

wasm32-wasi-cabal build exe:ormolu-live
wasm32-wasi-cabal list-bin exe:ormolu-live
ORMOLU_WASM="$(wasm32-wasi-cabal list-bin exe:ormolu-live)"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
  --input "$ORMOLU_WASM" --output "www/ghc_wasm_jsffi.js"

wizer \
    --allow-wasi --wasm-bulk-memory true \
    "$ORMOLU_WASM" -o "$WDIR/ormolu-init.wasm" \
    --mapdir /::../extract-hackage-info
if [ $# -eq 0 ]; then
    ORMOLU_WASM_OPT="$WDIR/ormolu-init.wasm"
else
    ORMOLU_WASM_OPT="$WDIR/ormolu-opt.wasm"
    wasm-opt "$@" "$WDIR/ormolu-init.wasm" -o "$ORMOLU_WASM_OPT"
    wasm-strip "$ORMOLU_WASM_OPT"
fi

rm -rf dist
mkdir -p dist
cp "$ORMOLU_WASM_OPT" dist/ormolu-live.wasm
esbuild www/index.js --bundle --minify --platform=browser --format=esm --outfile=dist/index.js
cp www/index.html dist/
cp node_modules/bulma/css/bulma.min.css dist/

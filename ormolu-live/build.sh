#!/usr/bin/env bash
set -e
WDIR="$(mktemp -d)"
trap 'rm -rf -- "$WDIR"' EXIT

if [ $# -eq 0 ]; then
    echo "Building for dev"
    dev_mode=true
else
    echo "Building for prod"
    dev_mode=false
fi

wasm32-wasi-cabal build exe:ormolu-live
wasm32-wasi-cabal list-bin exe:ormolu-live
ORMOLU_WASM="$(wasm32-wasi-cabal list-bin exe:ormolu-live)"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
  --input "$ORMOLU_WASM" --output "www/ghc_wasm_jsffi.js"

env PWD=/ wizer \
    --allow-wasi --wasm-bulk-memory true --inherit-env true \
    "$ORMOLU_WASM" -o "$WDIR/ormolu-init.wasm" \
    --mapdir /::../extract-hackage-info

if $dev_mode; then
    ORMOLU_WASM_OPT="$WDIR/ormolu-init.wasm"
else
    ORMOLU_WASM_OPT="$WDIR/ormolu-opt.wasm"
    wasm-opt "$WDIR/ormolu-init.wasm" -o "$ORMOLU_WASM_OPT" -Oz
    wasm-strip "$ORMOLU_WASM_OPT"
fi

rm -rf dist
mkdir -p dist
cp "$ORMOLU_WASM_OPT" dist/ormolu-live.wasm

esbuild_args=(--bundle --platform=browser --format=esm)
[[ $dev_mode == false ]] && esbuild_args+=(--minify)
esbuild www/index.js --outfile=dist/index.js "${esbuild_args[@]}"

cp www/index.html dist/
cp node_modules/bulma/css/bulma.min.css dist/

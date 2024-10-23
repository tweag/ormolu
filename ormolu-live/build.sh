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

git rev-parse HEAD > .commitrev

wasm32-wasi-cabal build
wasm32-wasi-cabal list-bin exe:ormolu-live
ORMOLU_WASM="$(wasm32-wasi-cabal list-bin exe:ormolu-live)"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
  --input "$ORMOLU_WASM" --output "www/ghc_wasm_jsffi.js"

if $dev_mode; then
    ORMOLU_WASM_FINAL="$ORMOLU_WASM"
else
    env PWD=/ wizer \
        --allow-wasi --wasm-bulk-memory true --inherit-env true \
        "$ORMOLU_WASM" -o "$WDIR/ormolu-init.wasm" \
        --mapdir /::../extract-hackage-info
    ORMOLU_WASM_FINAL="$WDIR/ormolu-opt.wasm"
    wasm-opt "$WDIR/ormolu-init.wasm" -o "$ORMOLU_WASM_FINAL" -Oz
    wasm-strip "$ORMOLU_WASM_FINAL"
fi

rm -rf dist
mkdir -p dist
cp "$ORMOLU_WASM_FINAL" dist/ormolu-live.wasm

esbuild_args=(--bundle --platform=browser --format=esm)
[[ $dev_mode == false ]] && esbuild_args+=(--minify)
esbuild www/index.js --outfile=dist/index.js "${esbuild_args[@]}"

wasmedge --dir /:./dist "$(wasm32-wasi-cabal list-bin exe:prerender)"
cp node_modules/bulma/css/bulma.min.css dist/

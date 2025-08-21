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
    wizer \
        --allow-wasi --wasm-bulk-memory true --init-func _initialize \
        "$ORMOLU_WASM" -o "$WDIR/ormolu-init.wasm"
    ORMOLU_WASM_FINAL="$WDIR/ormolu-opt.wasm"
    wasm-opt "$WDIR/ormolu-init.wasm" -o "$ORMOLU_WASM_FINAL" -Oz
    wasm-tools strip "$ORMOLU_WASM_FINAL" -o "$ORMOLU_WASM_FINAL"
fi

rm -rf dist
mkdir -p dist
cp "$ORMOLU_WASM_FINAL" dist/ormolu-live.wasm

wasmtime --dir .::/ "$(wasm32-wasi-cabal list-bin exe:pregen)" \
         www/jsaddle.js dist/index.html

esbuild_args=(--platform=browser --format=esm)
[[ $dev_mode == false ]] && esbuild_args+=(--minify)
esbuild www/{index,worker}.js --outdir=dist --bundle "${esbuild_args[@]}"
esbuild www/jsaddle.js --outdir=dist "${esbuild_args[@]}"

cp node_modules/bulma/css/bulma.min.css dist/

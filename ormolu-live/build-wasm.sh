#!/usr/bin/env bash
set -e
wasm32-wasi-cabal build exe:ormolu-live
wasm32-wasi-cabal list-bin exe:ormolu-live
ORMOLU_WASM="$(wasm32-wasi-cabal list-bin exe:ormolu-live).wasm"
if [ $# -eq 0 ]; then
    cp "$ORMOLU_WASM" src/ormolu.wasm
else
    wasm-opt "$@" "$ORMOLU_WASM" -o src/ormolu.wasm
fi

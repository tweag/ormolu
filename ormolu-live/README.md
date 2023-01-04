# Ormolu Live

Play around with Ormolu in the browser via the GHC WASM backend!

https://ormolu-live.tweag.io

## Overview

ATM, the GHC WASM backend only supports emitting [WASI binaries][WASI], which can be run in the browser via e.g. [browser_wasi_shim][] or the more fully-featured [wasmer-wasi][].

Hence, Ormolu Live consists of two parts:

 - A regular Purescript frontend (in `src/`) that displays input/output and manages options.
 - A background web worker (in this directory with the source in `app/Main.hs`) that formats Haskell source code via the WASM-compiled Ormolu.

## Development

Make sure to be in the `.#ormoluLive` Nix shell when entering `./ormolu-live`, e.g. conveniently via [nix-direnv][].

### Local interactive development

For building the WASM binary, run
```console
wasm32-wasi-cabal update
```
and then iterate by running something like
```console
watchexec -w app ./build-wasm.sh
```

For the Purescript frontend, you can run
```console
watchexec -w src purs-nix compile
```
and
```console
parcel www/index.html
```
in parallel. The latter command will display the URL to a dev server, usually http://localhost:1234.

### Building the site for deployment

First, build the components:
```console
nix build .#ormoluLive
wasm32-wasi-cabal update
./build-wasm.sh -Oz
```
Here, `-Oz` tells `wasm-opt` to aggressively optimize for code size.

Then, combine the two:
```console
cp -r --no-preserve=mode,ownership result/ site
cp src/ormolu.wasm site/ormolu.*.wasm
```
The self-contained site is now in `site`.

## Acknowledgements

https://github.com/monadfix/ormolu-live

[WASI]: https://wasi.dev/
[browser_wasi_shim]: https://github.com/bjorn3/browser_wasi_shim
[wasmer-wasi]: https://github.com/wasmerio/wasmer-js
[nix-direnv]: https://github.com/nix-community/nix-direnv

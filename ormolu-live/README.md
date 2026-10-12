# Ormolu Live

Play around with Ormolu in the browser via the GHC WASM backend!

https://ormolu-live.tweag.io

## Overview

We use the [Miso][] framework together with [jsaddle-wasm][]. Most of the code is in `src/Ormolu/Live.hs`. The WASM module is run in a web worker to avoid UI stuttering when formatting larger code chunks.

Additionally, we depend on a few packages via `npm` for JS and CSS, see `package.json`.

Finally, everything is brought together in `www/{index,worker}.js`.

## Building

Make sure to be in the `.#ormoluLive` Nix shell when entering `./ormolu-live`, e.g. conveniently via [nix-direnv][].

### Local interactive development

Install the `npm` dependencies:

```console
npm install
```

If necessary, update the index state:
```console
wasm32-wasi-cabal update
```

If you only want to get fast feedback on compilation failures, use
```console
ghcid -c 'wasm32-wasi-cabal repl'
```

If you actually want to interact with the site in a browser, run sth like
```console
watchexec -w src -w app -w ormolu-live.cabal ./build.sh
```
and serve the resulting build products via
```console
miniserve dist
```
and view the site at http://127.0.0.1:8080/index.html.

### Building the site for deployment

```console
npm ci
wasm32-wasi-cabal update
./build.sh prod
```

The self-contained site is now in `dist`.

## Acknowledgements

https://github.com/monadfix/ormolu-live

[Miso]: https://github.com/dmjio/miso
[browser_wasi_shim]: https://github.com/bjorn3/browser_wasi_shim
[jsaddle-wasm]: https://github.com/amesgen/jsaddle-wasm
[nix-direnv]: https://github.com/nix-community/nix-direnv

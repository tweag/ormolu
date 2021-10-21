# Ormolu Live

Play around with ormolu in the browser via GHCJS!

## Development

### Building the site with GHCJS

```
nix-build -A ormoluLive.website
```

### Local development with JSaddle

In a `nix-shell` (or if you have cabal installed), run

```
ghcid -r -W
```

and open `http://localhost:8080` in a Chromium-based browser.

## Acknowledgements

https://github.com/monadfix/ormolu-live

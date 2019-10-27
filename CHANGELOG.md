## Unreleased

* Switched to `ghc-lib-parser` instead of depending on the `ghc` package
  directly. This should allow us to use newest features of GHC while not
  necessarily depending on the newest version of the compiler. In addition
  to that Ormolu is now GHCJS-compatible.

* Fixed formatting of result type in closed type families. See [issue
  420](https://github.com/tweag/ormolu/issues/420).

* Fixed a minor inconsistency between formatting of normal and foreign type
  signatures. See [issue 408](https://github.com/tweag/ormolu/issues/408).

## Ormolu 0.0.1.0

* Initial release.

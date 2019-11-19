## Unreleased

* Switched to `ghc-lib-parser` instead of depending on the `ghc` package
  directly. This should allow us to use newest features of GHC while not
  necessarily depending on the newest version of the compiler. In addition
  to that Ormolu is now GHCJS-compatible.

* Now unrecognized GHC options passed with `--ghc-opt` cause Ormolu to fail
  (exit code 7).

* Fixed formatting of result type in closed type families. See [issue
  420](https://github.com/tweag/ormolu/issues/420).

* Fixed a minor inconsistency between formatting of normal and foreign type
  signatures. See [issue 408](https://github.com/tweag/ormolu/issues/408).

* Fixed a bug when comment before module header with Haddock was moved
  inside the export list. See [issue
  430](https://github.com/tweag/ormolu/issues/430).

* Empty `forall`s are now correctly preserved. See [issue
  429](https://github.com/tweag/ormolu/issues/429).

* Fixed [issue 446](https://github.com/tweag/ormolu/issues/446), which
  involved braces and operators.

* When there are comments between preceding Haddock (pipe-style) and its
  corresponding declaration they are preserved like this in the output
  instead of being shifted. To be clear, this is not a very good idea to
  have comments in that position because the Haddock will end up not being
  associated with the declarations. Issues
  [440](https://github.com/tweag/ormolu/issues/440) and
  [448](https://github.com/tweag/ormolu/issues/448).

## Ormolu 0.0.1.0

* Initial release.

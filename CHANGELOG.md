## Ormolu 0.0.3.1

* Fixed rendering of record updates with the record dot preprocessor syntax
  [Issue 498](https://github.com/tweag/ormolu/issues/498).

## Ormolu 0.0.3.0

* Fixed an issue related to unnecessary use of curly braces. [Issue
  473](https://github.com/tweag/ormolu/issues/473).

* Fixed the issue with formatting multi-way if when it happens to be a
  function applied to arguments [Issue
  488](https://github.com/tweag/ormolu/issues/488). This changed the way
  multi-line if is formatted in general.

* Added support for record dot pre-processor when used via the plugin.
  [Issue 486](https://github.com/tweag/ormolu/issues/486).

* Stopped hanging record constructors and improved placing
  potentially-hanging consturctions in the presence of comments. [Issue
  447](https://github.com/tweag/ormolu/issues/447).

* Fixed indentation in presence of type applications. [Issue
  493](https://github.com/tweag/ormolu/issues/493).

* Class and instance declarations now do not have a blank line after
  `where`. Grouping of methods from the original input is also preserved
  with some normalizations. [Issue
  431](https://github.com/tweag/ormolu/issues/431).

## Ormolu 0.0.2.0

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

* Implemented correct handling of shebangs. [Issue
  377](https://github.com/tweag/ormolu/issues/377).

* Implemented correct handling of stack headers. [Issue
  393](https://github.com/tweag/ormolu/issues/393).

* Sorting language pragmas cannot not change meaning of the input program
  anymore. [Issue 404](https://github.com/tweag/ormolu/issues/404).

* Fixed formatting of applications where function is a complex expression.
  [Issue 444](https://github.com/tweag/ormolu/issues/444).

## Ormolu 0.0.1.0

* Initial release.

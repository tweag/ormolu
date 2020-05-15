## Unreleased

* Fixed rendering of type signatures concerning several identifiers. [Issue
  566](https://github.com/tweag/ormolu/issues/566).

* Fixed an idempotence issue with inline comments in tuples and parentheses.
  [Issue 450](https://github.com/tweag/ormolu/issues/450).

* Fixed an idempotence issue when certain comments where picked up as
  “continuation” of a series of comments [Issue
  449](https://github.com/tweag/ormolu/issues/449).

* Fixed an idempotence issue related to different indentation levels in a
  comment series. [Issue 512](https://github.com/tweag/ormolu/issues/512).

* Fixed an idempotence issue related to comments which may happen to be
  separated from the elements they are attached to by the equality sign.
  [Issue 340](https://github.com/tweag/ormolu/issues/340).

* Fixed an idempotence issue with type synonym and data declarations where
  the type has a Haddock. [Issue
  578](https://github.com/tweag/ormolu/issues/578).

* Fix the false positive about AST differences in presence of comments with
  multiple blank lines in a row. [Issue
  518](https://github.com/tweag/ormolu/issues/518).

* Fixed rendering of comment around if expressions. [Issue
  458](https://github.com/tweag/ormolu/issues/458).

* Unnamed fields of data constructors are now documented using the `-- ^`
  syntax. [Issue 445](https://github.com/tweag/ormolu/issues/445) and [Issue
  428](https://github.com/tweag/ormolu/issues/428).

* Fixed non-idempotent transformation of partly documented data definition.
  [Issue 590](https://github.com/tweag/ormolu/issues/590).

* Fixed an idempotence issue related to operators. [Issue
  522](https://github.com/tweag/ormolu/issues/522).

* Renamed the `--check-idempotency` flag to `--check-idempotence`.
  Apparently only the latter is correct.

## Ormolu 0.0.5.0

* Grouping of statements in `do`-blocks is now preserved. [Issue
  74](https://github.com/tweag/ormolu/issues/74).

* Grouping of TH splices is now preserved. [Issue
  507](https://github.com/tweag/ormolu/issues/507).

* Comments on pragmas are now preserved. [Issue
  216](https://github.com/tweag/ormolu/issues/216).

* Ormolu can now be enabled and disabled via special comments. [Issue
  435](https://github.com/tweag/ormolu/issues/435).

* Added experimental support for simple CPP. [Issue
  415](https://github.com/tweag/ormolu/issues/415).

* Added two new options `--start-line` and `--end-line` that allow us to
  select a region to format. [Issue
  516](https://github.com/tweag/ormolu/issues/516).

* Fixed rendering of module headers in the presence of preceding comments or
  Haddocks. [Issue 561](https://github.com/tweag/ormolu/issues/561).

## Ormolu 0.0.4.0

* When given several files to format, Ormolu does not stop on the first
  failure, but tries to format all the files. [Issue
  502](https://github.com/tweag/ormolu/issues/502).

* Made rendering of pattern matches involving operators consistent with
  other cases of rendering of operators. [Issue
  500](https://github.com/tweag/ormolu/issues/500).

* More compact rendering of type family injectivity constraints. [Issue
  482](https://github.com/tweag/ormolu/issues/482).

* Improved rendering of the keyword `where` in class, data, and type family
  declarations. When headers of these declarations are multi-line `where` is
  now put on its own line. [Issue
  509](https://github.com/tweag/ormolu/issues/509).

* Fixed the bug pertaining to rendering of arrow notation with multiline
  expressions. [Issue 513](https://github.com/tweag/ormolu/issues/513).

* Made rendering of data type definitions, value-level applications, and
  application of types use the same style. Moreover, existential now doesn't
  cause the data constructor be unconditionally rendered in multiline
  layout. [Issue 427](https://github.com/tweag/ormolu/issues/427).

* Records with a single data constructor are now formatted more compactly.
  [Issue 425](https://github.com/tweag/ormolu/issues/425).

* Switched to `ghc-lib-parser-8.10.1`.

* Implemented support for the new language extensions `ImportQualifiedPost`
  and `StandaloneKindSignatures`.

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

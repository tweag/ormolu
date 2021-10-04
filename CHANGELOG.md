## Unreleased

* Allow check mode (`-m check`) when working with STDIN input ([Issue 634](
  https://github.com/tweag/ormolu/issues/634)).
* Now guards are printed on a new line if at least one guard is multiline or
  if all guards together occupy more than one line. The body of each guard
  is also indented one level deeper in that case. [Issue
  712](https://github.com/tweag/ormolu/issues/712).

## Ormolu 0.3.0.1

* Improvements to `.cabal` file handling:
   * When looking for a `.cabal` file, directories were previously
     erroneously also considered. [Issue 781](
     https://github.com/tweag/ormolu/issues/781).
   * We now print a note if Ormolu was told to consider
     `.cabal` files, but no suitable one could be found.
   * Handle an empty `hs-source-dirs` correctly.
   * Also consider modules which are only conditionally listed
     in the `.cabal` file.

* The special handling of CPP is now only applied if CPP is
  actually enabled. [Issue 782](https://github.com/tweag/ormolu/issues/782).

* The left hand side of the `:` operator now uses braces if
  necessary. [Issue 780](https://github.com/tweag/ormolu/issues/780).

## Ormolu 0.3.0.0

* Data declarations with multiline kind signatures are now formatted
  correctly. [Issue 749](https://github.com/tweag/ormolu/issues/749).

* Infix arrow command formations are formatted like usual operators.
  This fixes [Issue 748](https://github.com/tweag/ormolu/issues/748).

* `do` arrow commands are formatted more flexibly. Fixes [Issue
  753](https://github.com/tweag/ormolu/issues/753).

* Source code is always read and written using UTF8 and ignoring the native
  line ending conventions. [Issue
  717](https://github.com/tweag/ormolu/issues/717).

* Opt-in support to respect default-extensions and default-language
  from .cabal files. [Issue 517](https://github.com/tweag/ormolu/issues/517).

* Empty case expressions are now rendered with braces. [Issue
  765](https://github.com/tweag/ormolu/issues/765).

* Omit braces on repeated application of `do` blocks. [Issue
  735](https://github.com/tweag/ormolu/issues/735).

* Improved handling of disabled regions. [PR 773](
  https://github.com/tweag/ormolu/pull/773).
   * Disabled regions are now exactly preserved, in particular
     empty lines and trailing spaces. [Issue
     673](https://github.com/tweag/ormolu/issues/673).
   * Strings like `-}` can now be present in disabled regions.
     [Issue 708](https://github.com/tweag/ormolu/issues/708).

  This means that using CPP or magic comments in certain ways which
  were only supported as a side effect previously (like in
  [Issue 601](https://github.com/tweag/ormolu/issues/601))
  will now result in formatting failures. Also see [Issue 774](
  https://github.com/tweag/ormolu/issues/774).

## Ormolu 0.2.0.0

* Now standalone kind signatures are grouped with type synonyms. [Issue
  683](https://github.com/tweag/ormolu/issues/683).

* `TemplateHaskell` is not enabled by default anymore. [Issue
  699](https://github.com/tweag/ormolu/issues/699).

* Made record dot pre-processor rendering idempotent in certain specific
  cases. [Issue 701](https://github.com/tweag/ormolu/issues/701).

* Added support for arrow command application. [Issue
  716](https://github.com/tweag/ormolu/issues/716).

* Switched to `ghc-lib-parser-9.0.1`. [PR
  722](https://github.com/tweag/ormolu/pull/722).
   * Support for the new language extensions:
      * `LexicalNegation`, `LinearTypes`: disabled by default
      * `QualifiedDo`: enabled by default
   * Due to [upstream changes in whitespace sensitity](
     https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.0#whitespace-sensitive-and-),
     `TypeApplications` is now *enabled* by default. [Issue
     452](https://github.com/tweag/ormolu/issues/452).
   * Haddocks on declarations in files without a `module` header are no longer
     deleted. [Issue 480](https://github.com/tweag/ormolu/issues/480).
   * Due to a change in Haddock parsing, empty Haddock comments on function
     arguments now get deleted.

* CTYPE pragmas are now preserved. [Issue 689](
  https://github.com/tweag/ormolu/issues/689).

* `TypeApplications` in data/type family instances are now supported. [Issue
  698](https://github.com/tweag/ormolu/issues/698).

* Formatting infix arrow command formations now preserves the AST. [Issue
  718](https://github.com/tweag/ormolu/issues/718).

* Surround code in brackets with spaces if it contains a `StarIsType` `*` to
  prevent unparseable output. [Issue 704](https://github.com/tweag/ormolu/issues/704).

* Formatting applied multiline constructs in do blocks now preserves the AST.
  [Issue 707](https://github.com/tweag/ormolu/issues/707).

  This will sometimes result in odd indentations, e.g. this snippet is a
  fixed point:
  ```haskell
  foo = do
    do
      (+1)
     1
  ```

* GHC options passed in via the CLI can now be overridden in local files.
  Previously, if an extension was disabled via the CLI, it could not be
  re-enabled per file.

* `NegativeLiterals` is no longer enabled by default. Also, spaces after
  negation via `-` are removed where possible. [Issue
  694](https://github.com/tweag/ormolu/issues/694).

* Minus signs in literal patterns are now preserved in all cases. [Issue
  733](https://github.com/tweag/ormolu/issues/733).

* Added support for left-to-right arrow application. [Issue
  737](https://github.com/tweag/ormolu/issues/737).

* Now `--mode check` fails on missing trailing blank lines. [Issue
  743](https://github.com/tweag/ormolu/issues/743).

* Fixed indentation of arrow forms in do blocks. [Issue
  739](https://github.com/tweag/ormolu/issues/739).

## Ormolu 0.1.4.1

* Added command line option `--color` to control how diffs are printed.
  Standardized the way errors are printed.

## Ormolu 0.1.4.0

* Added support for monad comprehensions. [Issue
  665](https://github.com/tweag/ormolu/issues/665).

* Fixed a bug when a space was inserted in front of promoted types even when
  it wasn't strictly necessary. [Issue
  668](https://github.com/tweag/ormolu/issues/668).

* Now the checking mode displays diffs per file when unformatted files are
  found. The rendering of the diffs is also improved. [Issue
  656](https://github.com/tweag/ormolu/issues/656).

## Ormolu 0.1.3.1

* Fixed a problem with multiline record updates using the record dot
  preprocessor. [Issue 658](https://github.com/tweag/ormolu/issues/658).

## Ormolu 0.1.3.0

* Ormolu no longer overwrites already formatted files. [PR
  649](https://github.com/tweag/ormolu/pull/649).

* Now a space is guaranteed before ticked promoted types. [Issue
  631](https://github.com/tweag/ormolu/issues/631).

* Made formatting of single-line explicitly bidirectional pattern synonyms
  idempotent. [Issue 630](https://github.com/tweag/ormolu/issues/630).

## Ormolu 0.1.2.0

* Fixed the bug when comments in different styles got glued together after
  formatting. [Issue 589](https://github.com/tweag/ormolu/issues/589).

* Added `-i` as a shortcut for `--mode inplace`. [Issue
  467](https://github.com/tweag/ormolu/issues/467).

* Improved grouping of top-level declarations. [Issue
  466](https://github.com/tweag/ormolu/issues/466).

## Ormolu 0.1.1.0

* Imports in a import lists are now normalized: duplicate imports are
  combined/eliminated intelligently.

* Import declarations that can be merged are now automatically merged.
  [Issue 414](https://github.com/tweag/ormolu/issues/414).

* The magic comments for disabling and enabling Ormolu now can encompass any
  fragment of code provided that the remaining code after exclusion of the
  disabled part is still syntactically correct. [Issue
  601](https://github.com/tweag/ormolu/issues/601).

* Improved sorting of operators in imports. [Issue
  602](https://github.com/tweag/ormolu/issues/602).

* Fixed a bug related to trailing space in multiline comments in certain
  cases. [Issue 603](https://github.com/tweag/ormolu/issues/602).

* Added support for formatting linked lists with `(:)` as line terminator.
  [Issue 478](https://github.com/tweag/ormolu/issues/478).

* Fixed rendering of function arguments in multiline layout. [Issue
  609](https://github.com/tweag/ormolu/issues/609).

* Blank lines between definitions in `let` and `while` bindings are now
  preserved. [Issue 554](https://github.com/tweag/ormolu/issues/554).

* Fixed the bug when type applications stuck to the `$` of TH splices that
  followed them. [Issue 613](https://github.com/tweag/ormolu/issues/613).

* Improved region formatting so that indented fragments—such as definitions
  inside of `where` clauses—can be formatted. [Issue
  572](https://github.com/tweag/ormolu/issues/572).

* Fixed the bug related to the de-association of pragma comments. [Issue
  619](https://github.com/tweag/ormolu/issues/619).

## Ormolu 0.1.0.0

* Fixed rendering of type signatures concerning several identifiers. [Issue
  566](https://github.com/tweag/ormolu/issues/566).

* Fixed an idempotence issue with inline comments in tuples and parentheses.
  [Issue 450](https://github.com/tweag/ormolu/issues/450).

* Fixed an idempotence issue when certain comments were picked up as
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

* Fixed rendering of comments around if expressions. [Issue
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

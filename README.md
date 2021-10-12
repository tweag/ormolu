# Ormolu

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/ormolu.svg?style=flat)](https://hackage.haskell.org/package/ormolu)
[![Stackage Nightly](http://stackage.org/package/ormolu/badge/nightly)](http://stackage.org/nightly/package/ormolu)
[![Stackage LTS](http://stackage.org/package/ormolu/badge/lts)](http://stackage.org/lts/package/ormolu)
[![Build status](https://badge.buildkite.com/8e3b0951f3652b77e1c422b361904136a539b0522029156354.svg?branch=master)](https://buildkite.com/tweag-1/ormolu)

* [Installation](#installation)
* [Building from source](#building-from-source)
* [Usage](#usage)
    * [Editor integration](#editor-integration)
    * [GitHub actions](#github-actions)
    * [Magic comments](#magic-comments)
    * [Account for .cabal files](#account-for-cabal-files)
    * [Exit codes](#exit-codes)
* [Limitations](#limitations)
* [Running on Hackage](#running-on-hackage)
* [Contributing](#contributing)
* [License](#license)

Ormolu is a formatter for Haskell source code. The project was created with
the following goals in mind:

* Using GHC's own parser to avoid parsing problems caused by
  [`haskell-src-exts`][haskell-src-exts].
* Let some whitespace be programmable. The layout of the input influences
  the layout choices in the output. This means that the choices between
  single-line/multi-line layouts in certain situations are made by the user,
  not by an algorithm. This makes the implementation simpler and leaves some
  control to the user while still guaranteeing that the formatted code is
  stylistically consistent.
* Writing code in such a way so it's easy to modify and maintain.
* Implementing one “true” formatting style which admits no configuration.
* The formatting style aims to result in minimal diffs.
* Choose a style compatible with modern dialects of Haskell. As new Haskell
  extensions enter broad use, we may change the style to accommodate them.
* Idempotence: formatting already formatted code doesn't change it.
* Be well-tested and robust so that the formatter can be used in large
  projects.

## Installation

The [release page][releases] has binaries for Linux, macOS and Windows.

You can also install using `cabal` or `stack`:

```console
$ cabal install ormolu
$ stack install ormolu
```

Ormolu is also included in several package repositories. E.g., on Arch Linux,
one can use [the package on AUR][aur]:

```console
$ yay -S ormolu
```

## Building from source

The easiest way to build the project is with Nix:

```console
$ nix-build -A ormolu
```

Note that you will need to add [IOHK Hydra binary
cache][iohk-hydra-binary-cache], otherwise building may take a very long
time.

Alternatively, `stack` could be used as follows:

```console
$ stack build # to build
$ stack install # to install
```

To use Ormolu directly from GitHub with Nix, this snippet may come in handy:

```nix
let
  pkgs = import <nixpkgs> { };
  source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "c1d8a8083cf1492545b8deed342c6399fe9873ea"; # update as necessary
      # do not forget to update the hash:
      sha256 = "sha256-3XxKuWqZnFa9s3mY7OBD+uEn/fGxPmC8jdevx7exy9o=";
    };
in (import source {  }).ormoluExe # this is e.g. the executable derivation
```

## Usage

The following will print the formatted output to the standard output.

```console
$ ormolu Module.hs
```

Add `--mode inplace` to replace the contents of the input file with the
formatted output.

```console
$ ormolu --mode inplace Module.hs
```

Use `find` to format a tree recursively:

```console
$ ormolu --mode inplace $(find . -name '*.hs')
```

Or find all files in a project with `git ls-files`:

```console
$ ormolu --mode inplace $(git ls-files '*.hs')
```

To check if files are are already formatted (useful on CI):

```console
$ ormolu --mode check $(find . -name '*.hs')
```

#### :zap: Beware git's `core.autocrlf` on Windows :zap:
Ormolu's output always uses LF line endings. In particular,
`ormolu --mode check` will fail if its input is correctly formatted
*except* that it has CRLF line endings. This situation can happen on Windows
when checking out a git repository without having set [`core.autocrlf`](
https://www.git-scm.com/docs/git-config#Documentation/git-config.txt-coreautocrlf)
to `false`.

### Editor integration

We know of the following editor integrations:

* [Emacs][emacs-package]
* [VS Code][vs-code-plugin]
* Vim: [neoformat][neoformat], [vim-ormolu][vim-ormolu]

### GitHub actions

[`ormolu-action`][ormolu-action] is the recommended way to ensure that a
project is formatted with Ormolu.

### Magic comments

Ormolu understands two magic comments:

```haskell
{- ORMOLU_DISABLE -}
```

and

```haskell
{- ORMOLU_ENABLE -}
```

This allows us to disable formatting selectively for code between these
markers or disable it for the entire file. To achieve the latter, just put
`{- ORMOLU_DISABLE -}` at the very top. Note that for Ormolu to work the
fragments where Ormolu is enabled must be parseable on their own. Because of
that the magic comments cannot be placed arbitrarily, but rather must
enclose independent top-level definitions.

### Account for .cabal files

Many cabal and stack projects use `default-extensions` to enable GHC
language extensions in all source files. With the
`--cabal-default-extensions` flag, Ormolu will take them into consideration
during formatting.

When you format input from stdin, you can pass `--stdin-input-file` which
will give Ormolu the location of the Haskell source file that should be used
as the starting point for searching for a suitable .cabal file.

### Exit codes

Exit code | Meaning
----------|-----------------------------------------------
0         | Success
1         | General problem
2         | CPP used (deprecated)
3         | Parsing of original input failed
4         | Parsing of formatted code failed
5         | AST of original and formatted code differs
6         | Formatting is not idempotent
7         | Unrecognized GHC options
8         | Cabal file parsing failed
9         | Missing input file path when using stdin input and accounting for .cabal files
100       | In checking mode: unformatted files
101       | Inplace mode does not work with stdin
102       | Other issue (with multiple input files)

## Limitations

* CPP support is experimental. CPP is virtually impossible to handle
  correctly, so we process them as a sort of unchangeable snippets. This
  works only in simple cases when CPP conditionals surround top-level
  declarations. See the [CPP][design-cpp] section in the design notes for a
  discussion of the dangers.
* Input modules should be parsable by Haddock, which is a bit stricter
  criterion than just being valid Haskell modules.

## Running on Hackage

It's possible to try Ormolu on arbitrary packages from Hackage. For that
execute (from the root of the cloned repo):

```console
$ nix-build -A hackage.<package>
```

Then inspect `result/log.txt` for possible problems. The derivation will
also contain formatted `.hs` files for inspection and original inputs with
`.hs-original` extension (those are with CPP dropped, exactly what is fed
into Ormolu).

## Contributing

See [CONTRIBUTING.md][contributing].

## License

See [LICENSE.md][license].

Copyright © 2018–present Tweag I/O

[aur]: https://aur.archlinux.org/packages/ormolu
[contributing]: https://github.com/tweag/ormolu/blob/master/CONTRIBUTING.md
[design-cpp]: https://github.com/tweag/ormolu/blob/master/DESIGN.md#cpp
[emacs-package]: https://github.com/vyorkin/ormolu.el
[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[iohk-hydra-binary-cache]: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/#setting-up-the-binary-cache
[license]: https://github.com/tweag/ormolu/blob/master/LICENSE.md
[neoformat]: https://github.com/sbdchd/neoformat
[releases]: https://github.com/tweag/ormolu/releases
[ormolu-action]: https://github.com/marketplace/actions/ormolu-action
[vim-ormolu]: https://github.com/sdiehl/vim-ormolu
[vs-code-plugin]: https://marketplace.visualstudio.com/items?itemName=sjurmillidahl.ormolu-vscode

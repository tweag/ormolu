# Ormolu

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/ormolu.svg?style=flat)](https://hackage.haskell.org/package/ormolu)
[![Stackage Nightly](http://stackage.org/package/ormolu/badge/nightly)](http://stackage.org/nightly/package/ormolu)
[![Stackage LTS](http://stackage.org/package/ormolu/badge/lts)](http://stackage.org/lts/package/ormolu)
[![CI](https://github.com/mrkkrp/ormolu/actions/workflows/ci.yml/badge.svg)](https://github.com/mrkkrp/ormolu/actions/workflows/ci.yml)

*Ormolu gratefully acknowledges the support and contributions of
Mark Karpov during the period 2019–2026, who was the author and main maintainer of the package during
his employment at [tweag][Tweag].*

* [Installation](#installation)
* [Building from source](#building-from-source)
* [Usage](#usage)
    * [Ormolu Live](#ormolu-live)
    * [Editor integration](#editor-integration)
    * [Haskell Language Server](#haskell-language-server)
    * [GitHub Actions](#github-actions)
    * [Language extensions, dependencies, and fixities](#language-extensions-dependencies-and-fixities)
    * [Magic comments](#magic-comments)
    * [Regions](#regions)
    * [Exit codes](#exit-codes)
    * [Using as a library](#using-as-a-library)
* [Troubleshooting](#troubleshooting)
    * [Operators are being formatted weirdly!](#operators-are-being-formatted-weirdly)
* [Limitations](#limitations)
* [Running on Hackage](#running-on-hackage)
* [Forks and modifications](#forks-and-modifications)
* [Contributing](#contributing)
* [License](#license)

Ormolu is a formatter for Haskell source code. The project was created with
the following goals in mind:

* Use GHC's own parser to avoid the parsing problems caused by
  [`haskell-src-exts`][haskell-src-exts].
* Make some whitespace programmable. The layout of the input influences the
  layout choices in the output, so the choice between single-line and
  multi-line layouts is made by the user rather than by an algorithm. This
  keeps the implementation simpler and leaves some control to the user while
  still guaranteeing that the formatted code is stylistically consistent.
* Implement one “true” formatting style that admits no configuration.
* Produce minimal diffs.
* Choose a style compatible with modern dialects of Haskell. As new Haskell
  extensions enter broad use, we may adjust the style to accommodate them.
* Guarantee idempotence: formatting already formatted code doesn't change it.
* Stay well-tested and robust, so that the formatter can be used in large
  projects.

Try it out in your browser at <https://ormolu-live.markkarpov.com>!
See [Ormolu Live](#ormolu-live) for more info.

## Installation

The [release page][releases] has binaries for Linux, macOS, and Windows.

You can also install Ormolu with `cabal` or `stack`:

```console
$ cabal install ormolu
$ stack install ormolu
```

Ormolu is also included in several package repositories. For example, on Arch
Linux you can use [the package on AUR][aur]:

```console
$ yay -S ormolu
```

## Building from source

The easiest way to build the project is with Nix:

```console
$ nix build
```

Make sure to accept the offered Nix binary caches, otherwise building may
take a very long time. The flake declares the relevant caches (the IOG cache
and the project's own `ormolu.cachix.org`, which is populated by CI) via its
`nixConfig`, but Nix uses them only if you allow it to. The simplest way is
to pass `--accept-flake-config`:

```console
$ nix build --accept-flake-config
```

To avoid repeating the flag, add the following to your Nix configuration
(`/etc/nix/nix.conf`, or `nix.settings` on NixOS):

```
extra-substituters = https://cache.iog.io https://ormolu.cachix.org
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= ormolu.cachix.org-1:0L9Y4A+6dGpvfGtaeaq5w44pgX0AVRivKMfi2fiOzYE=
```

Alternatively, you can use `stack`:

```console
$ stack build # to build
$ stack install # to install
```

To use Ormolu directly from GitHub with Nix flakes, this snippet may come in
handy:

```nix
{
  inputs.ormolu.url = "github:mrkkrp/ormolu";
  outputs = { ormolu, ... }: {
    # use ormolu.packages.${system}.default here
  };
}
```

## Usage

The following prints the formatted output to the standard output:

```console
$ ormolu Module.hs
```

Add `--mode inplace` to replace the contents of the input file with the
formatted output:

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

To check whether files are already formatted (useful on CI):

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

### Ormolu Live

On every new commit to `master`, [Ormolu Live](./ormolu-live) is deployed to
https://ormolu-live.markkarpov.com. Older versions are available at
https://COMMITHASH--ormolu.netlify.app, where `COMMITHASH` is the hash of the
commit you want.

### Editor integration

We know of the following editor integrations:

* [Emacs][emacs-package]
* [VS Code][vs-code-plugin]
* Vim: [neoformat][neoformat], [vim-ormolu][vim-ormolu]

### Haskell Language Server

[Haskell Language Server](https://haskell-language-server.readthedocs.io)
has built-in support for using Ormolu as a formatter.

### GitHub Actions

[`run-ormolu`][run-ormolu] is the recommended way to ensure that a project
stays formatted with Ormolu.

### Language extensions, dependencies, and fixities

Ormolu automatically locates the Cabal file that corresponds to a given
source file. Cabal files are used to extract both default extensions and
dependencies. Default extensions directly affect the behavior of the GHC
parser, while dependencies are used to determine the fixities of operators
that appear in the source code. Fixities can also be overridden via an
`.ormolu` file, which should be located higher in the file system hierarchy
than the source file being formatted. When the input comes from stdin, you
can pass `--stdin-input-file` to tell Ormolu which location to use as the
starting point when searching for `.cabal` and `.ormolu` files.

Here is an example of an `.ormolu` file:

```haskell
infixr 9  .
infixr 5  ++
infixl 4  <$
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!
infixl 4 <*>, <*, *>, <**>

infixr 3 >~<
infixr 3.3 |~|
infixr 3.7 <~>
```

It uses exactly the same syntax as ordinary Haskell fixity declarations,
which makes it easier for Haskellers to edit and maintain. Since Ormolu
0.7.8.0, fractional precedences are supported for more precise control over
the formatting of complex operator chains.

As of Ormolu 0.7.0.0, `.ormolu` files can also contain instructions about
module re-exports that Ormolu should be aware of. This can be useful because
Ormolu cannot know about every possible module re-export in the ecosystem,
and only a few of them actually matter for fixity deduction. In 99% of cases
you won't have to do anything, especially since the most common re-exports
are already built into Ormolu. (You are welcome to open PRs to make Ormolu
aware of more re-exports by default.) However, when the fixity of an operator
is not inferred correctly, making Ormolu aware of a re-export may help. Here
is an example:

```haskell
module Control.Lens exports Control.Lens.At
module Control.Lens exports "lens" Control.Lens.Lens
```

Module re-export declarations can be mixed freely with fixity overrides, as
long as each declaration is on its own line. As of Ormolu 0.7.1.0 explicit
package names are allowed in re-export declarations (see the example above).

Finally, all of the above-mentioned parameters can be controlled from the
command line:

* Language extensions can be specified with the `-o` or `--ghc-opt` flag.
* Dependencies can be specified with the `-p` or `--package` flag.
* Fixities can be specified with the `-f` or `--fixity` flag.
* Re-exports can be specified with the `-r` or `--reexport` flag.

Searching for `.cabal` and `.ormolu` files can be disabled by passing
`--no-cabal` and `--no-dot-ormolu` respectively.

### Magic comments

Ormolu understands two magic comments:

```haskell
{- ORMOLU_DISABLE -}
```

and

```haskell
{- ORMOLU_ENABLE -}
```

These let you disable formatting selectively for the code between the two
markers, or for the entire file. To disable formatting for the whole file,
just put `{- ORMOLU_DISABLE -}` at the very top. Note that the fragments
where Ormolu is enabled must be parseable on their own. Because of this, the
magic comments cannot be placed arbitrarily; they must enclose independent
top-level definitions.

### Regions

You can ask Ormolu to format a region of the input and leave the rest
unformatted by passing the `--start-line` and `--end-line` command line
options. `--start-line` defaults to the beginning of the file, and
`--end-line` defaults to the end.

Note that the selected region needs to be parseable Haskell code on its own.

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
10        | Parse error while parsing fixity overrides
11        | Comments of original and formatted code differ
100       | In checking mode: unformatted files
101       | Inplace mode does not work with stdin
102       | Other issue (with multiple input files)

### Using as a library

The `ormolu` package can also be used as a dependency from other Haskell
programs. For this purpose, only the top-level `Ormolu` module should be
considered stable. It follows the [PVP](https://pvp.haskell.org/) starting
from version 0.5.3.0. Rely on other modules at your own risk.

## Troubleshooting

### Operators are being formatted weirdly!

This can happen when Ormolu doesn't know or can't determine the fixity of an
operator.

* If this is a custom operator, see the instructions in the [Language
  extensions, dependencies, and
  fixities](#language-extensions-dependencies-and-fixities) section to
  specify the correct fixities in a `.ormolu` file.

* If this is a third-party operator (e.g. from `base` or some other package
  on Hackage), Ormolu probably doesn't recognize that the operator is the
  same as the third-party one.

  Some possible reasons for this:

    * You have a custom Prelude that re-exports things from the standard
      Prelude.
    * You have `-XNoImplicitPrelude` turned on.

  If either of these applies, make sure to specify the re-exports correctly
  in a `.ormolu` file.

You can see how Ormolu decides the fixity of operators by using `--debug`.

## Limitations

* CPP support is experimental. CPP is virtually impossible to handle
  correctly, so Ormolu treats CPP sections as unchangeable snippets. This
  works only in simple cases, where CPP conditionals surround top-level
  declarations. See the [CPP][design-cpp] section of the design notes for a
  discussion of the dangers.

## Running on Hackage

You can try Ormolu on arbitrary packages from Hackage. To do so, run the
following from the root of the cloned repo:

```console
$ nix build .#hackage.<package>
```

Then inspect `result/log.txt` for possible problems. The derivation also
contains the formatted `.hs` files for inspection, along with the original
inputs under the `.hs-original` extension (these have CPP dropped and are
exactly what is fed into Ormolu).

## Forks and modifications

We know of the following actively maintained forks:

* [Fourmolu][fourmolu], which uses 4-space indentation and allows arbitrary
  configuration.

## Contributing

Contributions of all kinds are welcome, from bug reports and documentation
fixes to new features. Please see [CONTRIBUTING.md][contributing] to get
started.

## License

See [LICENSE.md][license].

Copyright © 2018 Tweag I/O, 2026–present Mark Karpov

[tweag]: https://tweag.io/
[aur]: https://aur.archlinux.org/packages/ormolu
[design-cpp]: https://github.com/mrkkrp/ormolu/blob/master/DESIGN.md#cpp
[emacs-package]: https://github.com/vyorkin/ormolu.el
[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[neoformat]: https://github.com/sbdchd/neoformat
[releases]: https://github.com/mrkkrp/ormolu/releases
[run-ormolu]: https://github.com/haskell-actions/run-ormolu
[vim-ormolu]: https://github.com/sdiehl/vim-ormolu
[vs-code-plugin]: https://marketplace.visualstudio.com/items?itemName=sjurmillidahl.ormolu-vscode
[fourmolu]: https://github.com/fourmolu/fourmolu
[contributing]: https://github.com/mrkkrp/ormolu/blob/master/CONTRIBUTING.md
[license]: https://github.com/mrkkrp/ormolu/blob/master/LICENSE.md

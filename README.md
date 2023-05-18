# Ormolu

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/ormolu.svg?style=flat)](https://hackage.haskell.org/package/ormolu)
[![Stackage Nightly](http://stackage.org/package/ormolu/badge/nightly)](http://stackage.org/nightly/package/ormolu)
[![Stackage LTS](http://stackage.org/package/ormolu/badge/lts)](http://stackage.org/lts/package/ormolu)
[![CI](https://github.com/tweag/ormolu/actions/workflows/ci.yml/badge.svg)](https://github.com/tweag/ormolu/actions/workflows/ci.yml)

* [Installation](#installation)
* [Building from source](#building-from-source)
* [Usage](#usage)
    * [Ormolu Live](#ormolu-live)
    * [Editor integration](#editor-integration)
    * [Haskell Language Server](#haskell-language-server)
    * [GitHub actions](#github-actions)
    * [Language extensions, dependencies, and fixities](#language-extensions-dependencies-and-fixities)
    * [Magic comments](#magic-comments)
    * [Regions](#regions)
    * [Exit codes](#exit-codes)
    * [Using as a library](#using-as-a-library)
* [Limitations](#limitations)
* [Running on Hackage](#running-on-hackage)
* [Forks and modifications](#forks-and-modifications)
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

Try it out in your browser at <https://ormolu-live.tweag.io>!
See [Ormolu Live](#ormolu-live) for more info.

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
$ nix build
```

Make sure to accept the offered Nix caches (in particular the IOG cache),
otherwise building may take a very long time.

Alternatively, `stack` could be used as follows:

```console
$ stack build # to build
$ stack install # to install
```

To use Ormolu directly from GitHub with Nix flakes, this snippet may come in handy:

```nix
{
  inputs.ormolu.url = "github:tweag/ormolu";
  outputs = { ormolu, ... }: {
    # use ormolu.packages.${system}.default here
  };
}
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

### Ormolu Live

On every new commit to `master`, [Ormolu Live](./ormolu-live) is deployed to
https://ormolu-live.tweag.io. Older versions are available at
https://COMMITHASH--ormolu-live.netlify.app.

### Editor integration

We know of the following editor integrations:

* [Emacs][emacs-package]
* [VS Code][vs-code-plugin]
* Vim: [neoformat][neoformat], [vim-ormolu][vim-ormolu]

### Haskell Language Server

[Haskell Language Server](https://haskell-language-server.readthedocs.io)
has built-in support for using Ormolu as a formatter.

### GitHub actions

[`run-ormolu`][run-ormolu] is the recommended way to ensure that a project
is formatted with Ormolu.

### Language extensions, dependencies, and fixities

Ormolu automatically locates the Cabal file that corresponds to a given
source code file. Cabal files are used to extract both default extensions
and dependencies. Default extensions directly affect behavior of the GHC
parser, while dependencies are used to figure out fixities of operators that
appear in the source code. Fixities can also be overridden via an `.ormolu`
file which should be located at a higher level in the file system hierarchy
than the source file that is being formatted. When the input comes from
stdin, one can pass `--stdin-input-file` which will give Ormolu the location
that should be used as the starting point for searching for `.cabal` and
`.ormolu` files.

Here is an example of `.ormolu` file:

```haskell
infixr 9  .
infixr 5  ++
infixl 4  <$
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!
infixl 4 <*>, <*, *>, <**>
```

It uses exactly the same syntax as usual Haskell fixity declarations to make
it easier for Haskellers to edit and maintain.

As of Ormolu 0.7.0.0, `.ormolu` files can also contain instructions about
module re-exports that Ormolu should be aware of. This might be desirable
because at the moment Ormolu cannot know about all possible module
re-exports in the ecosystem and only few of them are actually important when
it comes to fixity deduction. In 99% of cases the user won't have to do
anything, especially since most common re-exports are already programmed
into Ormolu. (You are welcome to open PRs to make Ormolu aware of more
re-exports by default.) However, when the fixity of an operator is not
inferred correctly, making Ormolu aware of a re-export may come in handy.
Here is an example:

```haskell
module Control.Lens exports Control.Lens.At
module Control.Lens exports Control.Lens.Lens
```

Module re-export declarations can be mixed freely with fixity overrides, as
long as each declaration is on its own line.

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

This allows us to disable formatting selectively for code between these
markers or disable it for the entire file. To achieve the latter, just put
`{- ORMOLU_DISABLE -}` at the very top. Note that for Ormolu to work the
fragments where Ormolu is enabled must be parseable on their own. Because of
that the magic comments cannot be placed arbitrarily, but rather must
enclose independent top-level definitions.

### Regions

One can ask Ormolu to format a region of input and leave the rest
unformatted. This is accomplished by passing the `--start-line` and
`--end-line` command line options. `--start-line` defaults to the beginning
of the file, while `--end-line` defaults to the end.

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
100       | In checking mode: unformatted files
101       | Inplace mode does not work with stdin
102       | Other issue (with multiple input files)

### Using as a library

The `ormolu` package can also be depended upon from other Haskell programs.
For these purposes only the top `Ormolu` module should be considered stable.
It follows [PVP](https://pvp.haskell.org/) starting from the version
0.5.3.0. Rely on other modules at your own risk.

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
$ nix build .#hackage.<package>
```

Then inspect `result/log.txt` for possible problems. The derivation will
also contain formatted `.hs` files for inspection and original inputs with
`.hs-original` extension (those are with CPP dropped, exactly what is fed
into Ormolu).

## Forks and modifications

We know of the following actively maintained forks:

* [Fourmolu][fourmolu], which uses 4-space indentation and allows arbitrary
  configuration.

## Contributing

See [CONTRIBUTING.md][contributing].

## License

See [LICENSE.md][license].

Copyright © 2018–present Tweag I/O

[aur]: https://aur.archlinux.org/packages/ormolu
[design-cpp]: https://github.com/tweag/ormolu/blob/master/DESIGN.md#cpp
[emacs-package]: https://github.com/vyorkin/ormolu.el
[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[neoformat]: https://github.com/sbdchd/neoformat
[releases]: https://github.com/tweag/ormolu/releases
[run-ormolu]: https://github.com/haskell-actions/run-ormolu
[vim-ormolu]: https://github.com/sdiehl/vim-ormolu
[vs-code-plugin]: https://marketplace.visualstudio.com/items?itemName=sjurmillidahl.ormolu-vscode
[fourmolu]: https://github.com/fourmolu/fourmolu
[contributing]: https://github.com/tweag/ormolu/blob/master/CONTRIBUTING.md
[license]: https://github.com/tweag/ormolu/blob/master/LICENSE.md

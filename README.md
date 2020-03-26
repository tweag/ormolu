# Ormolu

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/ormolu.svg?style=flat)](https://hackage.haskell.org/package/ormolu)
[![Stackage Nightly](http://stackage.org/package/ormolu/badge/nightly)](http://stackage.org/nightly/package/ormolu)
[![Stackage LTS](http://stackage.org/package/ormolu/badge/lts)](http://stackage.org/lts/package/ormolu)
[![Build status](https://badge.buildkite.com/8e3b0951f3652b77e1c422b361904136a539b0522029156354.svg?branch=master)](https://buildkite.com/tweag-1/ormolu)

Ormolu is a formatter for Haskell source code. The project was created with
the following goals in mind:

* Using GHC's own parser to avoid parsing problems caused by
  [`haskell-src-exts`][haskell-src-exts].
* Let some whitespace be programmable. The layout of the input influences
  the layout choices in the output. This means that the choices between
  single-line/multi-line layouts in each particular situation are made by
  the user, not by an algorithm. This makes the implementation simpler and
  leaves some control to the user while still guaranteeing that the
  formatted code is stylistically consistent.
* Writing code in such a way so it's easy to modify and maintain.
* Implementing one “true” formatting style which admits no configuration.
* That formatting style aims to result in minimal diffs while still
  remaining very close to “conventional” Haskell formatting people use.
* Choose a style compatible with modern dialects of Haskell. As new Haskell
  extensions enter broad use, we may change the style to accomodate them.
* Idempotence: formatting already formatted code doesn't change it.
* Be well-tested and robust to the point that it can be used in large
  projects without exposing unfortunate, disappointing bugs here and there.

## Building

The easiest way to build the project is with Nix:

```console
$ nix-build -A ormolu
```

Or with `cabal-install` from the Nix shell:

```console
$ nix-shell --run "cabal new-build"
```

Alternatively, `stack` could be used with a `stack.yaml` file as follows.

```console
$ cat stack.yaml
resolver: lts-14.3
packages:
- '.'

$ stack build
```

To use Ormolu directly from GitHub with Nix, this snippet may come in handy:

```nix
# This overlay adds Ormolu straight from GitHub.
self: super:

let source = super.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "de279d80122b287374d4ed87c7b630db1f157642"; # update as necessary
      sha256 = "0qrxfk62ww6b60ha9sqcgl4nb2n5fhf66a65wszjngwkybwlzmrv"; # as well
    };
    ormolu = import source { pkgs = self; };
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ormolu.ormoluCompiler}" = super.haskell.packages.${ormolu.ormoluCompiler}.override {
        overrides = ormolu.ormoluOverlay;
      };
    };
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

## Current limitations

* Does not handle CPP (wontfix, see [the design document][design]).
* Input modules should be parsable by Haddock, which is a bit stricter
  criterion than just being valid Haskell modules.
* Various minor idempotence issues, most of them are related to comments.

## Editor integration

We know of the following editor integrations:

* [Emacs][emacs-package]
* [VS Code][vs-code-plugin]
* vim: [neoformat][neoformat], [vim-ormolu][vim-ormolu]

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

[design]: https://github.com/tweag/ormolu/blob/master/DESIGN.md#cpp
[contributing]: https://github.com/tweag/ormolu/blob/master/CONTRIBUTING.md
[license]: https://github.com/tweag/ormolu/blob/master/LICENSE.md
[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[emacs-package]: https://github.com/vyorkin/ormolu.el
[vs-code-plugin]: https://marketplace.visualstudio.com/items?itemName=sjurmillidahl.ormolu-vscode
[vim-ormolu]: https://github.com/sdiehl/vim-ormolu
[neoformat]: https://github.com/sbdchd/neoformat

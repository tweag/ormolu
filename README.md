# Ormolu

[![CircleCI](https://circleci.com/gh/tweag/ormolu/tree/master.svg?style=svg&circle-token=cfd37a39265561eb44e608f97cf953cb2a394c03)](https://circleci.com/gh/tweag/ormolu/tree/master)

Ormolu is a formatter for Haskell source code. The project was created with
the following features in mind:

* Using GHC's own parser to avoid parsing problems caused by
  [`haskell-src-exts`][haskell-src-exts].
* Let some whitespace be programmable. The layout of the input
  influence the layout choices in the output. This means that the
  choices between single-line/multi-line layouts in each particular
  situation are made by the user, not by an algorithm. This makes the
  implementation simpler and leaves some control to the user while
  still guaranteeing that the formatted code is stylistically
  consistent.
* Writing code in such a way so it's easy to modify and maintain. Roughly,
  it means that the project follows the path of [`hindent`][hindent] and is
  very much about printing AST in a particular way.
* Implementing one “true” formatting style which admits no configuration.
* That formatting style aims to result in minimal diffs while still
  remaining very close to “conventional” Haskell formatting people use.
* Idempotency: formatting already formatted code doesn't change it.
* Be well-tested and robust to the point that it can be used in large
  projects without exposing unfortunate, disappointing bugs here and there.

## Building

The easiest way to build the project is with Nix:

```console
$ nix-build
```

Or with `cabal-install` from the Nix shell:

```console
$ nix-shell --run "cabal new-build"
```

Alternatively, `stack` could be used with a `stack.yaml` file as follows.

```console
$ cat stack.yaml
resolver: lts-13.19
packages:
- '.'

$ stack build
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

## Running on Hackage

It's possible to try Ormolu on arbitrary packages from Hackage. For that
execute:

```console
$ nix-build -A hackage.<package>
```

Then inspect `result/log.txt` for possible problems. The derivation will
also contain formatted `.hs` files for inspection and original inputs with
`.hs-original` extension (those are with CPP dropped, exactly what is fed
into Ormolu).

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md).

## License

See [LICENSE.md](./LICENSE.md).

Copyright © 2018–present Tweag I/O

[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[hindent]: https://hackage.haskell.org/package/hindent

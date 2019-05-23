# Ormolu

[![CircleCI](https://circleci.com/gh/tweag/ormolu/tree/master.svg?style=svg&circle-token=cfd37a39265561eb44e608f97cf953cb2a394c03)](https://circleci.com/gh/tweag/ormolu/tree/master)

Ormolu is a formatter for Haskell source code. The projects was created with
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

## Contribution

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this
project](https://github.com/tweag/ormolu/issues). Pull requests are also
welcome.

### What to hack on?

Right now there are two options for people who want to contribute:

* Implementing rendering of AST. This is the main focus right now because we
  want to have a MVP which can render all syntactical constructions found in
  Haskell source code. Once that is achieved we'll polish the tool
  iteratively.
* [Fixing bugs][bugs]. If this seems more interesting than implementing
  rendering of AST, you are welcome to do this as well.

### Implementing rendering of AST

The `Ormolu.Printer.Combinators` module provides a DSL for rendering of GHC
AST. You'll probably only need this one module for writing new rendering
functions. The module documents how to use the printing combinators it
provides. Consult the Haddocks to learn more about them.

Create new modules corresponding to the things you want to render under
`Ormolu.Printer.Meat`. For example, there are `Ormolu.Printer.Meat.Type` and
`Ormolu.Printer.Meat.Declaration.Data`.

Concrete rendering functions get their names by appending `p_` to the name
of the type which describes a particular part of AST. For example, the
function for rendering types is correspondingly:

```haskell
p_hsType :: HsType GhcPs -> R ()
```

In general rendering functions like this take 1 or more arguments and
produce `R ()` which is a rendering action.

### Testing

Testing has been taken good care of and now it amounts to just adding
examples under `data/examples`. Each example is a pair of files:
`<example-name>.hs` for input and `<example-name>-out.hs` for corresponding
expected output.

Testing is performed as following:

* Given snippet of source code is parsed and pretty-printed.
* The result of printing is parsed back again and the AST is compared to the
  AST obtained from the original file. They should match.
* The output of printer is checked against the expected output.
* Idempotency property is verified: formatting already formatted code
  results in exactly the same output.

Examples can be organized in sub-directories, see the existing ones for
inspiration.

## License

See [LICENSE](./LICENSE.md).

Copyright © 2018–2019 Tweag I/O

[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[hindent]: https://hackage.haskell.org/package/hindent
[bugs]: https://github.com/tweag/ormolu/issues?q=is%3Aissue+is%3Aopen+label%3Abug

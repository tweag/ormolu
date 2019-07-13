# Contributing

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this
project](https://github.com/tweag/ormolu/issues). Pull requests are also
welcome.

When contributing to this repository, please first discuss the change you
wish to make via an issue, unless it's entirely trivial (typo fixes, etc.).
If there is already an issue that describes the change you have in mind,
comment on it indicating that you're going to work on that. This way we can
avoid the situation when several people work on the same thing.

Please make sure that all non-trivial changes are described in commit
messages and PR descriptions.

## What to hack on?

* [Fixing bugs][bugs]. This is the main focus right now.

## Implementing rendering of AST

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

## CI

We use Circle CI. Some outside contributors may have problems, as in, CI
won't run for PRs opened from forks with “unauthorized” errors. In that case
the best we can do is to add you as a contributor or to restart your build
manually.

If you have been added as a contributor but the builds still do not start,
try clicking

```
User settings -> Account integrations -> Refresh permissions
```

in Circle CI app.

[bugs]: https://github.com/tweag/ormolu/issues?q=is%3Aissue+is%3Aopen+label%3Abug

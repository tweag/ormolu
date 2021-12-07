# Extract information about packages and operators from Hackage

## Introduction

In order to format chains of operator applications nicely, Ormolu needs to
be aware of the precedence level (0 to 9) and fixity direction of every
infix operator (what we will just call "fixity" in the rest of this
document).

In order to build a database of operator fixities, we choose to parse the
declarations of every package from Hackage/Hoogle, looking for things of the
form:

```haskell
(++) : [a] -> [a] -> [a]
```

(that we will call a symbol declaration)

or

```haskell
infixr 4 ++
```

(that we will call a fixity declaration)

The second kind of declaration can be exploited directly, because it links
an infix operator with a fixity and a source (the package from which the
declaration originates).

The first kind, however, doesn't give any fixity information directly. It is
only when there is no matching fixity declaration inside the package files
that a symbol declaration indicates that the operator has the default fixity
(`infixl 9`).

In addition to the extraction of operator fixities, we also scrap the
download count of the last 30 days for every package on Hackage, to get a
popularity metric for packages which will be used to arbitrate between
conflicting fixity declarations.

The `extract-hackage-info` executable takes care of everything listed above,
and generates a `hackage-info.json` file containing two associative maps:

+ package name &rarr; operator &rarr; fixity
+ package name &rarr; popularity score

## How to use `extract-hackage-info`

Two files need to be downloaded first:

+ the [Hoogle database](https://hackage.haskell.org/packages/hoogle.tar.gz)
+ the [package table from the Hackage website](
    https://hackage.haskell.org/packages/browse),
  which includes the 30-days download counts

It is possible to download both with the following commands:

```bash
mkdir -p hoogle-database && \
    curl https://hackage.haskell.org/packages/hoogle.tar.gz | \
    tar -xz -C hoogle-database
curl https://hackage.haskell.org/packages/browse -o hackage-database.html
```

Then, run (inside a `nix-shell` at the root of the `ormolu` project):

```bash
cabal run extract-hackage-info -- \
    <path/to/hoogle-database> \
    <path/to/hackage-database.html>
```

The output file will be generated at
`extract-hackage-info/hackage-info.json`, but it is also possible to use a
custom output path with the `-o <path/to/output.json>` option.

Finally, an option is provided to limit the number of items in each maps of
the output file, for test or debugging purposes: `-d <limit>`.

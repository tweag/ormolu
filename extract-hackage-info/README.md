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

In general, correct resolution of fixities requires taking into account the
import section of a module that is being formatted, as well as knowing the
provenance (that is, module name) of each operator. Therefore, we also
collect and save this information.

The `extract-hackage-info` executable takes care of everything listed above,
and generates a `hackage-info.bin` file containing multi-level map from
package names to module names to operators to their fixities:

```haskell
newtype HackageInfo
  = HackageInfo (Map PackageName (Map ModuleName (Map OpName FixityInfo)))
```

## How to use `extract-hackage-info`

Run `extract-hackage-info.sh` from the root of the repository.

You can also dump the Hackage info database as JSON via

```console
cabal run extract-hackage-info -- dump extract-hackage-info/hackage-info.bin
```

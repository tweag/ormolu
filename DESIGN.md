# Ormolu

* [Analysis of the existing solutions](#analysis-of-the-existing-solutions)
    * [Brittany](#brittany)
    * [Hindent](#hindent)
    * [Stylish Haskell](#stylish-haskell)
    * [Haskell formatter](#haskell-formatter)
* [Proposed design](#proposed-design)
    * [Parsing](#parsing)
    * [CPP](#cpp)
    * [Printing](#printing)
    * [Configuration](#configuration)
    * [Handling of language extensions](#handling-of-language-extensions)
    * [Testing](#testing)
    * [Functionality of executable](#functionality-of-executable)
    * [Why not contribute to/fork Hindent or Brittany?](#why-not-contribute-tofork-hindent-or-brittany)
* [Roadmap](#roadmap)

This document describes design of a new formatter for Haskell source code.
It also includes recommendations for future implementers.

## Analysis of the existing solutions

In order to design a new formatter we need to study the existing solutions
so we can borrow the good bits and avoid making the same mistakes.

### Brittany

[Brittany][brittany] builds on top of [`ghc-exactprint`][ghc-exactprint]—a
library that uses parser of GHC itself for parsing and thus it guarantees
that at least parsing phase is bug-free (which is admittedly the cause of
majority of bugs in other projects, see below).

`ghc-exactprint` is capable of dealing not only with vanilla Haskell source
code, but also with CPP. However it does so by running the preprocessor with
parameters provided by user (such as values of `#defines`). It doesn't parse
actual preprocessor directives in a way that would allow their
reconstruction, thus Brittany still lacks support for handling source code
with CPP in it. That said, we'll show later supporting CPP correctly is
virtually impossible anyway.

After parsing, Haskell AST and a collection of annotations are available.
The annotations are there because Haskell AST doesn't provide enough
information to reconstruct source code (for example it doesn't include
comments). Brittany's approach then amounts to manipulation of these
entities using a stateful monad in which very low-level transformations are
described.

After the manipulations a few functions from `ghc-exactprint` are used for
rendering of code together with some custom logic. I do not fully understand
why the function `exactPrint` from `ghc-exactprint` is not used directly.

The code is hard to read and is buggy too, as one would expect with such
approach. There are enough bugs in Brittany at the moment so that it's
hardly usable although it's now 2 years old. Looking at the opened bugs it's
clear that almost all of them are because of the too-low-level approach
which seems to be very fragile.

### Hindent

[Hindent][hindent] uses [`haskell-src-exts`][haskell-src-exts] for parsing
like all older projects. `haskell-src-exts` does not use parser of GHC
itself, and is a source of endless parsing bugs. `Hindent` is affected by
these upstream issues as well as Stylish Haskell and Haskell formatter (see
below). This already makes all these projects unusable with some valid
Haskell source code, but let's continue studying Hindent anyway.

Hindent is quite different from Brittany in the sense that it does not
attempt to do manipulations on source code and annotations to print them
afterwards, instead it just prints the parsed code straight away. This means
that maybe 70-80% of the code does printing, the package is essentially is
about pretty-printing parsed Haskell code.

Perhaps surprisingly, this approach seems to be better (IMO), for several
reasons:

1. Brittany cannot guarantee that the output will be “canonical”. With given
   configuration, if we pass it different inputs expressing identical
   Haskell programs modulo spacing and indentation, by design it may produce
   different outputs:

   ```
   input0   input1   ...   inputN
      |       |              |
   output0  output1  ...   outputN
   ```

   While with the approach taken by Hindent we get rather this:

   ```
   input0   input1   ...   inputN
     |        |              |
      \       |             /
       \      |            /
        \     |           /
         \    |          /
          \   |         /
           \  |        /
        canonical output
   ```

   Because there is one way to pretty print parsed program. This means that
   the users won't need to think about the layout at all because it'll be
   100% determined by the pretty-printer after the transformation.

2. The code is easier to read and debug. Pretty-printing functions are very
   straightforward. If there is a bug (in pretty-printer, not in parser
   which Hindent cannot control), it's easy to fix AFAIU.

Hindent is also notable for its ability to handle CPP and inputs that do not
constitute complete modules. It splits input stream into so-called “code
blocks” recognizing CPP macros and then only pretty-prints “normal code”
without touching CPP directives. After that CPP is inserted between
pretty-printed blocks of source code. The approach fails when CPP breaks
code in such a way that separate blocks do not form valid Haskell
expressions, see
[this](https://github.com/commercialhaskell/hindent/issues/383) for example.

Looking at the bug tracker there are many bugs. Part of them is because of
the use of `haskell-src-exts`, the other part is because the maintainer
doesn't care (anymore?) and doesn't fix them. Well it's as simple as that,
with any sort of commercial backing the bugs in pretty printer would be
fixed long time ago.

### Stylish Haskell

[Stylish Haskell][stylish-haskell] also uses `haskell-src-exts` and suffers
from the same upstream problems. I haven't studied the transformations it
performs, but it looks like it transforms the parsed source code partially
by manipulating AST and partially by manipulating raw text (e.g. to drop
trailing whitspace from each line). CPP Macros are just filtered out
silently as a preprocessing step before feeding the code to
`haskell-src-exts`.

Stylish Haskell is not so invasive as the other formatters and most reported
bugs are about parsing issues and CPP. As I understand it, people mostly use
it to screw their import lists.

### Haskell formatter

[Haskell formatter][haskell-formatter] is an older project that didn't get
much traction. It also uses `haskell-src-ext` and also tries to do
manipulations on the parsed AST. The issue tracker doesn't have many issues
probably because it never got popular enough (only 15 stars on GitHub). All
the issues are about upstream problems with `haskell-src-exts`.

## Proposed design

This section describes a solution that combines all the good things from the
projects above.

### Parsing and CPP

It is clear that `ghc-exactprint` is better than `haskell-src-exts`, so we
should use that. If we go with `ghc-exactprint` though, we'll need to
specify which parser to use, e.g. the parser that parses whole module or the
one which parsers declarations/expressions/etc. It seems that in general
it's better to use the parser for modules because it should work with all
input files containing complete modules, while with other parsers it's
impossible to guess what they'll be called on.

### CPP

Formatting a module which uses CPP directives won't be supported. Instead,
we hope for a solution to replace CPP to do conditional compilation.

There are the following challenges when formatting a module with CPP:

* GHC parser won't accept anything but a valid, complete module.

* To preserve CPP directives they must be either removed from input and
  saved in some sort of structure or turned into a special sort of comments
  and restored later, but that is not easy because then the formatting logic
  itself should somehow be aware of CPP directives (mainly conditiionals)
  and avoid performing transformations that may lead to incorrect Haskell in
  the end. This is much, much harder than simple pretty-printing we want to
  do.

Here is an example that proves that CPP is hard to support if possible at all.
Let's suppose that we want to format the following snippet of code:

```haskell
f = f1
  where
        f1 = g
#if C1
g = g1
  where
    g1 = g2
      where
        g2 = False
#else
        g = True
#end
```

After commenting CPP directives we get:

```haskell
f = f1
  where
        f1 = g
{-[#if C1]-}
g = g1
  where
    g1 = g2
      where
        g2 = False
{-[#else]-}
        g = True
{-[#end]-}
```

Then, after formatting we get:

```haskell
f = f1
  where
    f1 = g
{-[#if C1]-}
g = g1
  where
    g1 = g2
      where
        g2 = False
{-[#else]-}
        g = True
{-[#end]-}
```

And finally, we uncomment CPP directives:

```haskell
f = f1
  where
    f1 = g
#if C1
g = g1
  where
    g1 = g2
      where
        g2 = False
#else
        g = True
#end
```

Now the definition of `f` is broken when `C1` doesn't hold.

Therefore, CPP should not be supported. If the CPP extension
is enabled, we should signal an error right away.

### Printing

Just pretty-printing code (following the approach of Hindent) seems sane. It
is straightforward and when complemented with enough tests (see the section
about testing below), it should work all right.

Implementation can be just a `Reader` monad producing something like text
builder. The context of `Reader` can store current indentation and
configuration options.

As a pretty-printing library we can use [`Outputable`][outputable] (and
`SDoc`) from the [`ghc`][ghc] package itself. The benefit is that AST
components that we'll want to print are already instances of `Outputable`,
so we'll get correct renderings for free.

### Configuration

There should be some configuration but at this point I'll leave this section
empty. Personally I'm OK with imposing “one good style” and only allowing
users to tweak things like page width in columns and indentation levels.
Others will probably disagree. I generally like the philosophy of [this
post][hindent-5-blog] by Chris Done (the author of Hindent) which says that
as long as the default style is conventional and good it doesn't really
matter how code gets formatted. Consistency is more important.

### Handling of language extensions

Some language extensions affect how parsing is done. We are going to deal
with those in two ways:

* When language pragmas are present in source file, we must parse them
  before we run the main parser (I guess) and they should determine how the
  main parsing will be done.
* There also should be configuration file that may enable other language
  extensions to be used on all files.
* Later we could try to locate Cabal files and fetch the list of extensions
  that are enabled by default from there.

### Testing

It should be possible to add tests incrementally as we develop
pretty-printing code and new issues are discovered. For each Haskell
module that we want to test, we perform the following steps:

1. Given input snippet of source code parse it and pretty print it.
2. Parse the result of pretty-printing again and make sure that AST is the
   same as AST of original snippet module span positions. We could make
   this part of a self-check in the formatter.
3. Check the output against expected output. Thus all tests should include
   two files: input and expected output.
4. Check that running the formatter on the output produces the same output
   again (the transformation is idempotent).

In order to grow our testsuite, we would borrow test cases from test
suites of existing libraries like Brittany and Hindent.
Then we may add test cases for opened issues that Brittany and Hindent have.

When we're confident enough, we can start “mining” new issues by running
the program on real source code from Hackage till we don't get new issues
anymore. For every issue that we find this way, a test case should be added.

### Functionality of executable

* In all cases the program should test if the produced AST is the same as
  the one we originally parsed and if it differs, an error should be
  displayed suggesting reporting this on our issue tracker.
* Check mode: return non-zero exit code if any transformations would be
  applied.
* Modification in place and printing of formatted code to stdout.
* A flag for version/commit information.
* An option to specify location of config file.
* Options to specify parameters that come from config files on command line
  instead, for example indent levels, maximal line width, etc.

### Why not contribute to/fork HIndent or Brittany?

* Forking or contributing to Brittany is not a good idea because this would
  require re-doing of all transformation logic, which is harder than writing
  pretty-printing code from scratch. Good documentation would help
  readability of the code somewhat, but then we would need to spend time
  either collaborating with the original author or investigating how
  everything works ourselves.

  Of course with sufficient persistence we could succeed in fixing
  Brittany's bugs, but the point is that pretty-printing à la Hindent is
  more maintainable (IMO) and switching to that is equal to re-doing the
  project.

  In the end, design of Ormolu is going to be simpler and will:

  * make the project more maintainable
  * bugs easier to fix
  * more people will be able to contribute to our project (I couldn't figure
    out what is going on in Brittany, so I'd not be able to contribute, but
    I could contribute to Hindent because I understand how it works even
    though I spent equal amount of time looking at both)

* Forking or contributing to Hindent is not an option because if we replace
  `haskell-src-exts` with `ghc` (or `ghc-exact-print`) then we'll have to
  work with a different AST type and all the code in Hindent will become
  incompatible and there won't be much code to be re-used in that case. It
  is also possible that we'll find a nicer way to write pretty-printer.

## Roadmap

Proposed roadmap (for a single person, about 39 full-time work days):

* ~~Create and setup a repo, setup CI (less than 1 day).~~
* ~~Implement parsing (2 days).~~
* Implement basis for pretty-printing of code. We could take
  [this][hindent-printer] as inspiration, although AST from the `ghc`
  package may be slightly different. Also, don't forget about annotations
  and comments (1 week).
* Implement executable program so we can easier fool around. Try it on
  simple code samples (2 days).
* Implement location and reading of configuration file in YAML format (2
  days).
* Continue writing the pretty-printing code till we cover everything. Start
  adding tests, see [the section about testing](#testing). Fix bugs. At this
  point just repeat this till we cover everything that Brittany and Hindent
  can do maybe by stealing their tests (at least 2 weeks).
* Start checking how our formatter works on real-world code from Hackage.
  Fix bugs that we find that way (2 weeks).
* Write a good readme explaining why the project was created and how it is
  better, etc (2 days).
* Improve the executable by adding more interesting options (2 days).
* Switch CI to Travis and test with multiple GHC versions (1 day).
* Make and announce the first public release on Twitter/Reddit (1 day).
* Figure out how to use the formatter application with major text editors
  and add the info to the readme (no estimate).

[brittany]: https://hackage.haskell.org/package/brittany
[hindent]: https://hackage.haskell.org/package/hindent
[hindent-5-blog]: https://chrisdone.com/posts/hindent-5
[stylish-haskell]: https://hackage.haskell.org/package/stylish-haskell
[haskell-formatter]: https://hackage.haskell.org/package/haskell-formatter
[ghc]: https://hackage.haskell.org/package/ghc
[outputable]: https://hackage.haskell.org/package/ghc-8.4.3/docs/Outputable.html
[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[ghc-exactprint]: https://hackage.haskell.org/package/ghc-exactprint
[hindent-printer]: https://github.com/commercialhaskell/hindent/blob/master/src/HIndent/Pretty.hs

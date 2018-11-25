# Ormolu

* [Analysis of the existing solutions](#analysis-of-the-existing-solutions)
    * [Brittany](#brittany)
    * [Hindent](#hindent)
    * [Stylish Haskell](#stylish-haskell)
    * [Haskell formatter](#haskell-formatter)
* [Proposed design](#proposed-design)
    * [Parsing and CPP](#parsing-and-cpp)
    * [Printing](#printing)
    * [Configuration](#configuration)
    * [Handling of language extensions](#handling-of-language-extensions)
    * [Testing](#testing)
* [Roadmap](#roadmap)

This document describes design of a new formatter for Haskell source code.
It also includes some recommendations for future implementers.

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
with CPP in it. Which renders it almost unusable straight away.

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
which seems to be very fragile. I'm surprised it even works at all, probably
the author had strong will power.

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

1. Brittany cannot guarantee that the output will be “canoncial”. With given
   config, if we pass it different inputs expressing identical Haskell
   programs modulo spacing and indentation, by design it may produce
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

   Because there is one way to pretty print a parsed program. This means
   that the users won't need to think about the layout at all because it'll
   be 100% determined by the pretty-printer after the transformation.

2. The code is easier to read and debug. Pretty-printing functions are very
   straightforward. If there is a bug (in pretty-printer, not in parser
   which Hindent cannot control), it's easy to fix AFAIK.

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

In this section I describe a solution that combines all the good things from
the ones above and tries to solve the CPP problem better.

### Parsing and CPP

It is clear that `ghc-exactprint` is better than `haskell-src-exts`, so we
should use that. If we go with `ghc-exactprint` though, we'll need to
specify which parser to use, e.g. the parser that parses whole module or the
one which parsers declarations/expressions/etc. It seems that in general
it's better to use the parser for modules because it should work with all
input files containing complete modules, while with other parsers it's
impossible to guess what they'll be called on.

So far so good, but CPP problem remains. We cannot ignore it as does Stylish
Haskell, and cannot error out on CPP as does Brittany, and Hident solves the
problem only partially and we cannot follow that path because
`ghc-exactprint` won't accept anything but a complete module, as noted
above.

One workaround is to do the following:

1. Replace CPP with comments containing first some magic word and then the
   actual CPP directive.
2. Run GHC parser on that as usual. That will succeed and preserve the
   comments in annotations.
3. When printing, detect these special comments and output them as CPP
   directives again.

The approach above may present some difficulties though:

* When conditional CPP directives are turned into comments it's likely that
  we'll end up with invalid Haskell if code from several conditional
  branches is present at the same time.
* Restoring CPP properly and inserting it back may be triciier than we
  think.

### Printing

Just pretty-printing code (following the approach of Hindent) seems sane. It
is straightforward and when complemented with enough tests (see the section
about testing below), it should work all right.

Implementation can be just a `Reader` monad producing something like text
builder. The context of `Reader` can store current indentation and
configuration options.

Returning to `ghc-exactprint`, if we'll be printing the AST ourselves it
makes sense to depend on the [`ghc`][ghc] package directly.

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
pretty-printing code and new issues are discovered. The logic is
straightforward:

1. Given input snippet of source code parse it and pretty print.
2. Check the output against expected output. Thus all tests should include
   two files: input and expected output.
3. Check that running the formatter on the output produces the same output
   again (the transformation is idempotent). This also checks that the
   output is still acceptable for the GHC parser.
4. It's a good idea to steal test cases from test suites of existing
   libraries like Brittany and Hindent.
5. After that we may add test cases for opened issues that Brittany and
   Hindent have.
6. When we're confident enough we can start “mining” new issues by running
   the program on real source code from Hackage till we don't get new issues
   anymore. For every issue that we find this way a test case should be
   added.

## Roadmap

Proposed roadmap (for a single person, about 38 full-time work days):

* Create and setup a repo, setup CI (less than 1 day).
* Implement parsing, for now without CPP handling (1 day).
* Implement basis for pretty-printing of code. We could take
  [this][hindent-printer] as inspiration, although AST from the `ghc`
  package may be slightly different. Also, don't forget about annotations
  and comments (1 week).
* Implement executable program so we can easier fool around. Try it on
  simple code samples (1 day).
* Continue writing the pretty-printing code till we cover everything. Start
  adding tests, see [the section about testing](#testing). Fix bugs. At this
  point just repeat this till we cover everything that Brittany and Hindent
  can do maybe by stealing their tests (at least 2 weeks).
* Add support for CPP as described above. Also add the corresponding tests.
  Check that we're doing OK in the cases that are described in opened issues
  on the issue trackers of Brittany/Hindent/Stylish Haskell (1 week).
* Start checking how our formatter works on real-world code from Hackage.
  Fix bugs that we find that way (2 weeks).
* Write a good readme explaining why the project was created and how it is
  better, etc (1 day).
* Make and announce the first public release on Twitter/Reddit (less than 1
  day).
* Improve the executable by adding more interesting options, see other
  projects for inspiration. For example, it would be good to have an option
  which could check if running the formatter would change anything (1-3
  days).
* Figure out how to use the formatter application with major text editors
  and add the info to the readme (no estimate).

[brittany]: https://hackage.haskell.org/package/brittany
[hindent]: https://hackage.haskell.org/package/hindent
[hindent-5-blog]: https://chrisdone.com/posts/hindent-5
[stylish-haskell]: https://hackage.haskell.org/package/stylish-haskell
[haskell-formatter]: https://hackage.haskell.org/package/haskell-formatter
[ghc]: https://hackage.haskell.org/package/ghc
[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[ghc-exactprint]: https://hackage.haskell.org/package/ghc-exactprint
[hindent-printer]: https://github.com/commercialhaskell/hindent/blob/master/src/HIndent/Pretty.hs

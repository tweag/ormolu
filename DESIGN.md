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

We set for the following goals (mostly taken from
[brittany](https://github.com/lspitzner/brittany)):
* Preserve the meaning of the formatted functions;
* Make reasonable use of screen space;
* Use linear space and computation time on the size of the input;
* Preserve comments;
* Be idempotent.

## Analysis of the existing solutions

In order to design a new formatter we need to study the existing solutions
so we can borrow the good bits and avoid making the same mistakes.

### Brittany

[Brittany][brittany] builds on top of [`ghc-exactprint`][ghc-exactprint]—a
library that uses parser of GHC itself for parsing and thus it guarantees
that at least parsing phase is bug-free (which is admittedly the cause of
majority of bugs in other projects, see below).

After parsing, Haskell AST and a collection of annotations are available.
The annotations are there because Haskell AST doesn't provide enough
information to reconstruct source code (for example it doesn't include
comments). The AST and the annotations are converted into a `BriDoc` value.
A `BriDoc` value is a document representation like the `Doc` from the
[pretty][pretty-doc] or the [wl-pprint][wl-pprint-doc] libraries.

Brittany implements its own document type in an attempt to find a
satisfactory rendering of the source code that fits a page-width
constraint. Because of this, a `BriDoc` value represents a collection
of many candidate layouts (i.e. renderings) of the source code.

This collection is pruned until it contains a single layout. The
structure of the chosen layout is then adjusted to leave it in a
form which can be easily traversed to produce the final rendering.

Brittany invests the majority of its implementation to manage
the `BriDoc` values. Given that the amount of possible layouts is
exponential, the representation is clever enough to fit them in
linear space. There are multiple ways to build a `BriDoc`, not all
of which fit in linear space. So care is necessary to keep memory
bounded.

The compexities of the `BriDoc` structure, together with the lack of
documentation, make Brittany at least challenging to maintain.

### Hindent

[Hindent][hindent] uses [`haskell-src-exts`][haskell-src-exts] for parsing
like all older projects. `haskell-src-exts` does not use parser of GHC
itself, and is a source of endless parsing bugs. `Hindent` is affected by
these upstream issues as well as Stylish Haskell and Haskell formatter (see
below). This already makes all these projects unusable with some valid
Haskell source code, but let's continue studying Hindent anyway.

Hindent is quite different from Brittany in the sense that it does not
attempt to build a document representation to render
afterwards, instead it just prints the parsed code straight away. This means
that the 70-80% of what the code does is a printing traversal.

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

* GHC parser won't accept anything but a valid, complete module. Therefore,
  formatting the Haskell code between CPP directives is not an option.

* Ignoring the CPP directives and formatting the Haskell code can change
  its meaning. An example follows.

Let's suppose that we want to format the following program:

```
$ cat test.hs
{-# LANGUAGE CPP #-}
main = print (g && f1)
  where
        f1 = h
          where
            h = True
#ifdef C1
g = g1
  where
    g1 = g2
      where
        g2 = False
#else
        g = True
#endif

#ifndef C1
g = False
#endif

$ runhaskell test.hs
True
```

At the time of this writing, formatting this program with Hindent
produces the same output we would get if the CPP directives were
considered comments:

```
$ hindent --version
hindent 5.2.7

$ hindent test.hs

$ cat test.hs
{-# LANGUAGE CPP #-}

main = print (g && f1)
  where
    f1 = h
      where
        h = True
#ifdef C1
g = g1
  where
    g1 = g2
      where
        g2 = False
#else
        g = True
#endif

#ifndef C1
g = False
#endif

$ runhaskell test.hs
False
```

Running the formatter causes the output of the program to change
from `True` to `False` when `C1` is not defined.

A solution could be to make the formatter more careful with CPP
directives, constraining how directives can be inserted in Haskell
code to avoid changing the meaning by reformatting. But
this would introduce additional complexity, and the problem would
need to be solved repeteadly for every tool out there which wants
to parse Haskell modules. If CPP is replaced with some language
extension or mechanism to do conditional compilation, all tools
will benefit from it.

Therefore, CPP won't be supported. If the CPP extension
is enabled, we should signal an error right away.

### Printing

Just pretty-printing code (following the approach of Hindent) seems sane. It
is straightforward and when complemented with enough tests (see the section
about testing below), it should work all right.

Implementation can be just a `Reader` monad producing something like text
builder. The context of `Reader` can store current indentation and
configuration options.

As the pretty-printing library we can use [`Outputable`][outputable] (and
`SDoc`) from the [`ghc`][ghc] package itself (at least for pretty-printing
basic things like floating point literals and the like). The benefit is that
AST components that we'll want to print are already instances of
`Outputable`, so we'll get correct renderings for free.

In order to keep the output of the formatter simple, fast and correct,
we introduce the following rule. The pretty-printing code can be in
control of every formatting choice, except for two, which are left to
the programmer:

1. location of comments (comments are going to be attached to specific
   syntactic entities, so moving an entity will move its comment too),
2. line breaking.

Regarding (2), the idea is that given any syntactic entity, the programmer
has a choice:

1. write it on one line, or
2. write it on two lines or more.

If (1), then everything is kept in one line. If (2), i.e. a line break appears
somewhere in the concrete syntax tree (CST), then additional line breaks are
introduced everywhere possible in parent nodes, *but not in any sibling or
children nodes*.

Examples:

```haskell
-- Stays as is.
data T = A | B

data T
  = A | B
-- Is reformatted to:
data T
  = A
  | B

-- Stays as is.
map :: (a -> b) -> [a] -> [b]

foldr :: (a -> b -> b) ->
  b -> [a] -> [b]
-- Is reformatted to:
foldr
  :: (a -> b -> b)
  -> b
  -> [a]
  -> [b]

t = let x = foo bar
                      baz
  in foo bar baz
-- Is reformatted to:
t =
    let x =
          foo
            bar
            baz
    in foo far baz
```

Crucially, no effort is made to fit within reasonable line lengths. That's
up to the programmer. Style will still be consistent for everyone in every
other aspect, and that's what counts.

Not having to worry about line lengths is a huge simplification. Worrying
about them is why Hindent can run in time exponential to the size of the
input: it tries two variants at each node of the CST, to see whether one of
them fits on one 80-character line (and selects that one if so). If your CST
is even just somewhat deep, then you’re going to be waiting on your
formatter for a while.

### Configuration

There should be some configuration but at this point I'll leave this section
empty. Personally I'm OK with imposing “one good style” and only allowing
users to tweak indentation levels. Others will probably disagree. I generally
like the philosophy of [this
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

We want to simultaneously optimize three goals:

1. simplicity of implementation,
2. efficiency,
3. predictable and readable output that doesn't overuse vertical spacing.

Hindent optimizes for (1) and gives up on (2) and (3). Brittany gives up on
(1) but goes a long way towards (3) and presumably does OK on (2). Ormolu
goes for (1)-(3), by outsourcing the hard part of (3) to the user. Ormolu is
less normative than Brittany, and less normative than Hindent, but arguably
stills achieves consistent style.

Forking or contributing to Hindent is not an option because if we replace
`haskell-src-exts` with `ghc` (or `ghc-exact-print`) then we'll have to work
with a different AST type and all the code in Hindent will become
incompatible and there won't be much code to be re-used in that case. It is
also possible that we'll find a nicer way to write pretty-printer.

## Examples

A list of formatting examples can be found [here](EXAMPLES.md).

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
[pretty-doc]: http://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint.html#t:Doc
[wl-pprint-doc]: http://hackage.haskell.org/package/wl-pprint-1.2.1/docs/Text-PrettyPrint-Leijen.html#t:Doc

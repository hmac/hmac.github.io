---
title: Implementing A Functional Language
---
# Implementing A Functional Language

This is the first in a series of posts on implementing a simple functional language. What
we'll focus on is the evaluation step: how code is executed on the processor. A typical
functional language compiler goes through the following steps to turn the code you write
into a binary you run:

```
               PARSE  TYPECHECK  DESUGAR      COMPILE            CODEGEN
[your code] -> [ast] -> [ast] -> [core] -> [abstract IR] -> [backend language]
```

We will look at the transformation from `[core]` to `[abstract IR]`. In Haskell, this
corresponds to the transformation from GHC Core to STG. This process is interesting
because it is here that we define _how the program is evaluated_. We will look at several
different designs for compilers, of increasing complexity, and consider problems such as
laziness, sharing, garbage collection, strictness analysis and parallel evaluation.

This series is heavily based on a fantastic book of the same name written by Simon Peyton
Jones and David Lester in 2000. All credit goes to them for the content: I've simply
altered the presentation and updated it slightly. There are a number of additional
differences:

- The book uses Miranda as its implementation language, whereas we will use Haskell.
- Some time is spent in the book on introducing parser combinators and pretty printing,
  whereas we will not focus on these them. Parsing and pretty printing are topics in their
  own right, and there are better introductions to them elsewhere.
- No external libraries are used in the book, whereas we will use a few common ones in the
  interest of brevity.
- We will spend time on building a framework for visualising the evaluation process, as I
  think that makes it easier to understand how the compiler works.

The book is freely available in PDF form [here][0]. There's also a broader overview of
this area (again by Simon Peyton Jones) available [here][1]. I highly recommend both of
them.

## Structure

The structure of this series is as follows. However this is a work in progress, so I may
change things as I complete each section.

1. [The Core Language][part1]
2. Template Instantiation
3. The G-machine
4. The Three Instruction Machine
5. The Parallel G-machine
6. Lambda Lifting
7. The Spineless Tagless G-machine

[0]: https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/
[1]: https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/
[part1]: 2019-03-03-the-core-language.html

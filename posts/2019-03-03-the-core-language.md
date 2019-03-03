---
title: "Implementing A Functional Language Part I: The Core Language"
---

This is part 1 of a series in implementing a functional language. The introduction is
[here][intro].

# The Core Language

Let's look at the language we're going to compile. It should look familiar if you've used
Haskell or another ML-like language before.

This is a simple Core program that evaluates to 6:
```haskell
main = addTwo 4
addTwo n = n + 2
```

This is a more complex program showcasing all of Core's features:
```haskell
main = let f = g Pack{1,0}
        in f 4
-- Lines starting like this are comments
g a b = case a of
          <1> -> b + 1
          <2> -> b + 2
```

We'll briefly describe each in turn.

## Expressions and application

All valid constructs are expressions - they evaluate to a result. Functions are applied to
arguments via juxtaposition: `f 1 2`. A Core program consists of a series of
_definitions_. Definitions are delineated by a semicolon `;`. Semicolons are also used
where necessary in other constructs. You'll see examples of this as we go on.

## Global definitions

Functions can be defined at the global level, like `g` in the example above. The syntax is the
same as any ML-like language.
```haskell
functionName arg1 ... argN = functionBody
```
where `arg1 ... argN` are bound in `functionBody`.

Values can be defined similarly, resembling nullary functions.
```haskell
five = 5
seven = five + 2
```

A valid Core program must have a nullary top level definition called `main`. This is the
entrypoint of the program.

## Local definitions

`let` statements allow defining values in a local scope.
```haskell
f = let a = 4
        double x = x + x
     in double a
```

`letrec` statements are similar, but allow recursive definitions. We make the distinction
between the two because non-recursive `let`s are often easier to implement and we can take
an approach that leads to more performant code than the recursive case[^1].

## Data Types

In Haskell, a data type might be defined as follows:
```haskell
data Colour = Red | Green | Blue
data Coord = MkCoord Int Int
data Tree a
  = Leaf a
  | Branch (Tree a)
           (Tree a)
```

The first case defines a type with three distinct nullary constructors. The second
case defines a type with a single constructor taking two arguments. The third case
combines these two, and is also parameterised over an abstract type `a`.

In Core we assume that typechecking has already happened, and so we don't need to
distinguish between constructors of different types. The constructor names are also not
important[^2]. Core doesn't support polymorphism, so type parameters don't need to be
considered. As such, we can simplify data type declaration. In Core, all type constructors
take the form `Pack{t,n}` where `t` is the _tag_ of the constructor and `n` is the _arity_
of the constructor. The tag is an integer starting at 1 and distinguishes between
constructors of the same type. The arity of a consntructor is the number of arguments it
takes. For example,
```haskell
data Colour = Red | Green | Blue
```
can be represented as
```haskell
Red = Pack{1,0}
Green = Pack{2,0}
Blue = Pack{3,0}
```

Similarly,
```haskell
data Tree
  = Leaf Int
  | Branch Tree Tree
```
translates to
```haskell
Leaf = Pack{1,1}
Branch = Pack{2,2}
```

## Case expressions

We construct data with `Pack`, and deconstruct it with `case`. This is the only way to
inspect data in Core, and subsumes all forms of pattern matching in the higher level
language (e.g. multiple function definitions)[^3]. Case expressions look like this:
```haskell
isLeaf tree = case tree of
  <1> v -> True ;
  <2> t1 t2 -> False
```

A `case` expression consists of a _scrutinee_ (`tree` in the example) followed by one or more
_alternatives_ separated by a semicolon. Alternatives take the form `<t> x1 ... xn -> e`
where `t` is the tag of the constructor and `x1 ... xn` are the arguments to the
constructor. The number of arguments must match the arity of the constructor. If the tag
of the constructor on the scrutinee matches the tag on the alternative, the whole case
expression evaluates to `e`. Every constructor must have a matching alternative - the
behaviour when a scrutinee's tag has no corresponding alternative is not defined (our
program will probably just crash).

## Built-in primitives

Core has built in support for integers, and a small set of arithmetic and boolean
operations defined as binary functions. The representation of Booleans themselves will
vary between implementations - often we will map them to the integers 0 and 1.

Operation                 Symbol
----------               -------
Addition                  `+`   
Subtraction               `-`   
Multiplication            `*`   
Division                  `/`   
Equality                  `==`  
Greater than              `>`   
Greater than or equal to  `>=`  
Less than                 `<`   
Less than or equal to     `<=`  
And                       `&`   
Or                        `|`   

Integer negation is performed via the primitive function `negate`.

[^1]: The higher-level source language might have a single unified `let` construct, with
  an analysis phase to determine which cases must be converted to `letrec` in Core.
[^2]: Except for providing useful error messages. Since you'd have to build this support
  into the compiler for your higher level language, we don't consider it here.
[^3]: Compiling more expressive pattern matching into case expressions is a well studied
  problem and there are established algorithms for doing so. For more information on this,
  see Chapter 5 of [The Implementation of Functional Programming Languages][book].

[intro]: 2019-03-02-implementing-a-functional-language.html
[book]: https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/

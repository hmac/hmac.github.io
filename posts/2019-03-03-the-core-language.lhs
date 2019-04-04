---
title: "Implementing A Functional Language Part I: The Core Language"
publish: true
---

Implementing a Functional Language I: The Core Language
=======================================================

This is part 1 of a series in implementing a functional language. The introduction is
[here][intro]. This is a literate Haskell file - you can download the source
[here](https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-03-the-core-language.lhs).
To load it into GHCi and play around, you can use the following command:
```
stack --resolver lts-12.2 \
      ghci --package prettyprinter \
           --package text \
      2019-03-03-the-core-language.lhs
```

---

> {-# LANGUAGE OverloadedStrings #-}
> module Core.Language where
> import Data.Text (Text)

The Core Language
=================

Let's look at the language we're going to compile. It should look familiar if you've used
Haskell or another ML-like language before.

This is a simple Core program that evaluates to 6:
```
main = addTwo 4 ;
addTwo n = n + 2
```

This is a more complex program showcasing all of Core's features:
```
main = let f = g Pack{1,0}
        in f 4 ;
-- Lines starting like this are comments
g a b = case a of
          <1> -> b + 1 ;
          <2> -> b + 2
```

We'll briefly describe each in turn.

Expressions and application
---------------------------

All valid constructs are expressions - they evaluate to a result. Functions are applied to
arguments via juxtaposition: `f 1 2`. A Core program consists of a series of
_definitions_. Definitions are delineated by a semicolon `;`. Semicolons are also used
where necessary in other constructs. You'll see examples of this as we go on.

Global definitions
------------------

Functions can be defined at the global level, like `g` in the example above. The syntax is the
same as any ML-like language.
```
functionName arg1 ... argN = functionBody
```
where `arg1 ... argN` are bound in `functionBody`.

Values can be defined similarly, resembling nullary functions.
```
five = 5 ;
seven = five + 2
```

A valid Core program must have a nullary top level definition called `main`. This is the
entrypoint of the program.

Local definitions
-----------------

`let` statements allow defining values in a local scope.
```
f = let a = 4 ;
        double x = x + x
     in double a
```

`letrec` statements are similar, but allow recursive definitions. We make the distinction
between the two because non-recursive `let`s are often easier to implement and we can take
an approach that leads to more performant code than the recursive case[^1].

Data Types
----------

In Haskell, a data type might be defined as follows:
```
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
constructors of the same type. The arity of a constructor is the number of arguments it
takes. For example,
```
data Colour = Red | Green | Blue
```
can be represented as
```
Red = Pack{1,0} ;
Green = Pack{2,0} ;
Blue = Pack{3,0}
```

Similarly,
```
data Tree
  = Leaf Int
  | Branch Tree Tree
```
translates to
```
Leaf = Pack{1,1} ;
Branch = Pack{2,2}
```

Case expressions
----------------

We construct data with `Pack`, and deconstruct it with `case`. This is the only way to
inspect data in Core, and subsumes all forms of pattern matching in the higher level
language (e.g. multiple function definitions)[^3]. Case expressions look like this:
```
isLeaf tree = case tree of
  <1> v -> True ;
  <2> t1 t2 -> False
```

A `case` expression consists of a _scrutinee_ (`tree` in the example) followed by one or more
_alternatives_ separated by a semicolon. Alternatives take the form
```
<t> x1 ... xn -> e
```
where `t` is the tag of the constructor and `x1 ... xn` are the arguments to the
constructor. The number of arguments must match the arity of the constructor. If the tag
of the constructor on the scrutinee matches the tag on the alternative, the whole case
expression evaluates to `e`. Every constructor must have a matching alternative - the
behaviour when a scrutinee's tag has no corresponding alternative is not defined (our
program will probably just crash).

Built-in primitives
-------------------

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

Lambda abstractions
-------------------

Notably absent from Core is _lambda abstraction_, or anonymous functions. Functions are
always supercombinators. This is an explicit choice to simplify the implementation, and we
do not lose any expressive power as a result. Lambda abstractions can be mechanically
removed from a program through a process known as _lambda lifting_. We will cover this in
more detail in later section.

Modelling the language
======================

Each implementation of the compiler will take as input a representation of a Core program,
so we need to define what that is. Everything else will be built around this type.

> data Expr a
>   = EVar Name            -- variables
>   | ENum Int             -- numbers
>   | EConstr              -- constructor
>             Int          --   tag
>             Int          --   arity
>   | EAp (Expr a)         -- applications
>         (Expr a)
>   | ELet                 -- let(rec) expressions
>          Recursive       --   recursive (letrec) or nonrecursive (let)
>          [(a, Expr a)]   --   definitions
>          (Expr a)        --   body
>   | ECase                -- case expressions
>           (Expr a)       --   expression to scrutinise
>           [Alter a]      --   alternatives
>   deriving (Show)
> 
> data Recursive = Recursive | NonRecursive
>   deriving (Show)
> 
> type Name = Text

`Expr` describes an expression in the Core language. It is parameterised over the type of
its _binders_ - a binder is a name given to a variable on the left hand side of a
`let(rec)` expression or function definition. This will allow us to model variable binding
more sophisticatedly in later parts without changing the definition of `Expr`. For now we
can make do with simple binders, so we define a type synonym for convenience.

> type CoreExpr = Expr Name

Function application is modelled by the `EAp` constructor. Applications of more than one
argument are transformed into nested `EAp` nodes.
```
f 1 2
-- becomes
EAp (EAp (EVar "f") (ENum 1)) (ENum 2)
```

Case expressions consist of a scrutinee and a list of alternatives, modelled by `Alter`,
which is a tuple of the constructor tag, the constructor arguments and the result
expression.

> type Alter a = (Int, [a], Expr a)
> --               0    1    2
> 
> --            case s of
> --              [0] [1]   [2]
> --              <1> x y -> e

A _supercombinator_ is a function with no free variables - in Core all global definitions
are supercombinators.

> type ScDefn a = (Name, [a], Expr a)
> type CoreScDefn = ScDefn Name

A Core program is a collection of supercombinator definitions, one of which is called
"main".

> type Program a = [ScDefn a]
> type CoreProgram = Program Name

We can now define a complete Core program, as an example.

> -- main = double 21 ;
> -- double x = x + x
> exampleProgram :: CoreProgram
> exampleProgram
>   = [
>       ("main", [], EAp (EVar "double") (ENum 21))
>     , ("double", ["x"], EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))
>     ]

Parsing and Pretty Printing
===========================

We want to be able to parse and pretty print Core, but this is not a parsing or pretty
printing tutorial. Instead of going through this in detail, we will rely on the
[`Parse`][parse] and [`Print`][print] modules. Each of these are defined in literate
Haskell files and you can read them if you want to. However I'd recommend seeking out a
proper introduction to these topics if you're not familiar with them. If I come across or
recall any good ones I may link to them here for reference. The [book][book] also has an
extensive section on each of these topics, though the style is somewhat outdated.

[^1]: The higher-level source language might have a single unified `let` construct, with
  an analysis phase to determine which cases must be converted to `letrec` in Core.
[^2]: Except for providing useful error messages. Since you'd have to build this support
  into the compiler for your higher level language, we don't consider it here.
[^3]: Compiling more expressive pattern matching into case expressions is a well studied
  problem and there are established algorithms for doing so. For more information on this,
  see Chapter 5 of [The Implementation of Functional Programming Languages][book].

[intro]: 2019-03-02-implementing-a-functional-language.html
[book]: https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/
[parse]: 2019-03-03-parsing-core.html
[print]: 2019-03-03-printing-core.html

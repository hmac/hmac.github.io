---
title: Teaching Functional Programming
publish: true
---

# Teaching Functional Programming

I once TA’ed an undergraduate functional programming course and it was the
saddest thing. In came around sixty smart students, at least some of whom were eager to
learn. Over the course of 10 weeks, the only thing anyone learned was that
functional programming is a) hard, b) useless and c) not something they want to
learn anymore.

None of these are true! But the course was structured and taught to maximise
confusion. Since then I’ve been thinking a lot about how FP could be taught
better, so that students understand it and enjoy it.

The problems come down to:

- bad tooling
- confusing syntax
- many ways to do the same thing

Unfortunately all of these rhyme with “Haskell”, which was the language of choice.

## Getting set up

The worst way to spend your first workshop on any topic is to be wrestling with
installers and package managers, trying to get the language set up.

Haskell has improved in this regard in recent years, but there are still
multiple ways to install it (Stack, ghcup, Haskell Platform (?), OS package
managers, Nix) and multiple ways to create and build a project (Stack, cabal,
Nix, plain GHC). Newcomers don’t care about any of this. They just want to start
writing some code.

A good teaching language should not even have an “install” step, or it should be
minimal. It should be possible to write code in your browser and run it. To run
code locally, you should just download a single executable that does everything.
You should not need to “create a project” to get started.

## Syntax

The main goal of syntax in a teaching language is to communicate a concept
whilst avoiding confusion. Similar things should look similar and distinct
things should look distinct. In Haskell there are two main ways to define a
function:

```haskell
f = \x -> …
f x = …
```

This causes so much confusion. Students usually get that the function is called
`f`, but struggle to understand that the `x` on the left of the `=` and the
one the right are the same thing. It gets worse when you introduce
multi-equation definitions:

```haskell
f 0 = …
f 1 = …
f x = …
```

Are these different functions? What does it mean to put numbers before the equals sign?

Students will often add an equation to a function in a different part of the
file, resulting in something like

```haskell
f 0 = …
g z = …
f x = …
```

And then Haskell would complain that they’ve defined `f` twice. But they just
learned that multiple equations are not separate functions! Etc.

Syntax should try to be permissive whilst remaining predictable. In other words,
indentation sensitivity is terrible for teaching languages. I don’t think my
students ever truly understood when to indent a line and by how much. They would
often just hammer the spacebar until the error disappeared.

Juxtaposition as application (ie applying `f` to `x` by writing `f x`) requires you to
be careful with other syntax to avoid ambiguity. I think this style is
beneficial enough that it’s worth making other forms more verbose. If you went
the `f(x)` route then you could make different decisions here.

The scope of an expression should be clear to read and simple to write. An
obvious way to do this, which is familiar to many who have used JS or C or Java,
is braces. For example:

```haskell
f = { x -> … }
```

The braces visually delimit the scope of `x`. 

You can apply the same approach to all forms that introduce bindings:

```haskell
let x = 1 { x + 5 }

case x {
  Some y -> …,
  None -> …
}
```

Another advantage to this is that it is easier to parse, and so we can more
easily leverage tools like tree-sitter to provide syntax highlighting,
formatting etc. 

## Types

It’s crucial to understand types, but they are also a source of confusion.
Students often conflate type names with constructor names (both are uppercase).
It is particularly confusing that the list type has special syntax which is
similar but not the same as the term-level list syntax. Students will do things
like write `[Int, Int]` for the type of a two-element list and wonder why it
doesn’t work.

Constructors and types should be distinguishable. The Rust approach of prefixing
constructors with their type (`MyType::MyConstructor`) is a good one, but I don’t
think it’s the only option. Some languages [use colour](http://docs.idris-lang.org/en/latest/reference/semantic-highlighting.html)
to distinguish between types and terms, which is an interesting approach.

Type classes are a complex feature, and still controversial amongst experienced
programmers. For beginners, I think they are totally inappropriate. Even worse,
in Haskell you cannot avoid running into them if you write string or numeric
literals.

I don’t believe a teaching language should have type classes. Literals should
have a concrete type, so error messages are understandable. Ad-hoc polymorphism
can be achieved in other ways, but may not even be necessary for a teaching
language. 

The first use case for type classes that I run into with a new language is
printing a value to a string. If the language is able to inspect values at
runtime and print them, we can add a built in `to_string` function and avoid
introducing type classes.

Similarly, having a single arbitrary-sized integer type helps mitigate the need
for overloaded numeric literals.

## Laziness

Laziness is undoubtedly a useful feature, but isn’t pedagogically essential. It
also complicates the explanation of evaluation, and can result in unintuitive
runtime behaviour. Strict evaluation is more likely to be familiar, and allows
us to spend more time teaching the fundamental concepts. Lazy evaluation, lazy
algorithms and data structures etc. is a good topic for an advanced course.

## Debugging

Pure languages like Haskell are infamously hard to debug in the traditional way.
Even armed with hacks like `unsafePerformIO`, laziness makes it hard to reason
about the ordering of code.

A teaching language should have a built-in runtime debugger. You should be able
to pause evaluation at any point, inspect variables, and step into and out of
functions. Not only does this help diagnose problems, it’s also a good way to
show how evaluation works.

Debugging in general is a hugely under-invested area. Most programming languages
don't even have a debugger, and the state of the art in mainstream languages has
not advanced in decades. Taking inspiration from [recent research](https://dl.acm.org/doi/abs/10.1145/3290327)
and the work of people like [Bret Victor](http://worrydream.com/#!2/LadderOfAbstraction),
we can imagine much more ambitious debugging environments which allow the
student to explore and interact with their program as it is running.

## Everything else

Arguably the most important feature of a teaching language is what it lacks.
Students will inevitably search Google and Stack Overflow for solutions and
help. With an old, general purpose language like Haskell, they’ll find a zoo of
different libraries, techniques and language features. They will wonder what
language extensions they should enable, what packages to install, and what
advanced type system features to use.

None of this is helpful in learning the core concepts that we are trying to
teach. Having an entirely new language with no optional extras bolted on allows
us to sidestep this problem altogether.

## Syllabus

What should be taught, and in what order? I think the focus should be on the
essentials: data types and functions. With that in mind, a possible syllabus
could be:

1. Expressions, let, case, function application
2. Functions
3. Function types 
4. Data types: Sums and products
5. Polymorphic functions
6. Polymorphic data types

At this point I think you could stop. We’ve covered all the essential aspects of
typed functional programming which is common to almost every language in the
family. Further time should be spent applying these concepts by tackling more
complex problems. Learning how to effectively use a functional programming
language is often overlooked. For example, how immutable data structures are
designed, efficient purely functional algorithms, type-driven design. 

One topic we haven’t touched at all is side effects. Arguably this is an
omission, but i believe this is such a rabbit hole that it might be better
avoided entirely, and left to a more advanced course. Introducing side effects
to a pure language requires some kind of effects system, be that monadic IO or
something more sophisticated as in [Koka](https://koka-lang.github.io/koka/doc/index.html).
Neither is easy to understand for beginners. In particular, I don’t believe it
is appropriate to introduce category-theoretic concepts to beginners. And the
presence of a good interpreter and debugger should obviate the need for most
side effects.

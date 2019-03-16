---
title: "Implementing A Functional Language Part II: Graph Reduction"
---

Implementing a Functional Language II: Graph Reduction
======================================================

This is part 2 of a series in implementing a functional language. The introduction is
[here][intro].

---

Every compilation strategy we will cover in this series will be based on the same core
concept: lazy graph reduction. This idea is simple but powerful, and it's what we'll
introduce in this section.

A functional program is really just a single large expression which evaluates to a result.
We can model it as a tree of nested applications. For example, the expression `f 1 2`
looks like this:

```
@
| \
|  \
@   2
| \
|  \
f   1
```

We use the symbol `@` to denote an application node. Note that though `f` takes two
arguments, we apply each in turn via currying. Here's a more complicated example - the
tree of `(+ (* 2 3) 4)`:

```
@
| \
|  \
@   4
| \
|  \
+   @
    | \
    |  \
    @   3
    | \
    |  \
    *   2
```

To evaluate the program, we'll _reduce_ the tree. This process turns it into a graph,
because it some cases a node will have more than one edge pointing to it. Not all
expressions can be reduced: `+ 1 2` reduces to `3` but `3` does not reduce any further. An
expression which is reducible is known as a reducible expression, or redex. To evaluate an
entire program, all we need to do is repeatedly identify the next redex to reduce, and
then reduce it.

Let's try to reduce the graph above. We start at the top, with an application node `@`.
The child nodes are `@` (another application) and `4`. We can't immediately evaluate this
expression, so we proceed down into the child application node, which itself has children
`+` and `@`. `+` takes two arguments, in this case `(* 3 2)` and `4`. `+` also requires
that its arguments are both numbers, so we need to continue to reduce the first argument.
We descend into the right hand application node - the root of `(* 3 2)`. Here's where we
are in the graph:

```
@
| \
|  \
@   4
| \
|  \
+   @  <-------- we are here
    | \
    |  \
    @   3
    | \
    |  \
    *   2
```

As before, we descend into the child application node, finally reaching `*`. `*` takes two
numeric arguments, and we have two arguments in `2` and `3`. We've found a reducible
expression! Specifically, the subgraph

```
@†
| \
|  \
@   3
| \
|  \
*   2
```

can be reduced to the single numeric node `6`. The node marked with a † is the _root_ of
the redex. Performing the reduction, the graph now looks like this:

```
@
| \
|  \
@   4
| \
|  \
+   6
```

Proceeding back up graph, we can see that the arguments to `+` are now both numbers, so
we've found another redex. Reducing this, we end up with a single node `10`, which is the
result of the whole expression.

Selecting the next redex
------------------------

In the previous example example our two functions `+` and `*` required both of their
arguments to be evaluated to numbers before they could be applied, but this is not a
general rule. Take for example the function `K x y = x`: `x` and `y` could be an
arbitrarily large, unevaluated expression and we would still be able to reduce an
application of `K`. Visually:

```
@†----
|     \
|      \
@       @---1
| \      \
|  \      \
K   @      @---2
    | \     \
    |  \     \
    @   2     @---3
    | \        \
    |  \        \
    +   1        ...
```

reduces to

```
@
| \
|  \
@   2
| \
|  \
+   1
```

By choosing to reduce the application of `K` before reducing its arguments, we completely
avoided having to evaluate the large expression on the right hand side of the tree. This
is the essence of lazy evaluation: function arguments are evaluated _when needed_. By
changing how we select the next redex, we can profoundly alter the semantics of our
language.

Core is a lazy language, so we will adopt the following rules:
- we always select the _outermost_ redex. This corresponds to the outermost function
  application.
- for some built in primitives (such as the arithmetic operators) we will require that
  inner redexes are reduced before reducing the redex of the primitive.

Reducing programs
-----------------

TODO: normal form, head normal form, weak head normal form

Let's walk through the evaluation of the following program:
```
square x = * x x ;
main = square (square 3)
```

To start with, the graph of our program consists of just the node `main`. `main` is a
supercombinator with no arguments, so it is a redex. We reduce it by replacing it with its
body, yielding the following graph:

```
@
| \
|  \
|   @---3
|    \
|     \
|      square
square
```

The outermost redex is the top node - the outer application of `square`. To reduce it, we
replace the redex with an instance of the function body, substituting any parameters with
a pointer to the argument of the application. This gives us:

```
@
| \
|  \
|   @---3
|   |\
|   | \
|   |  square
@---
|
|
*
```

We see that the inner redex `(square 3)` is now shared between two application nodes. The
application of `*` cannot be reduced because `*` is a primitive and requires both its
arguments to be evaluated first. Hence the only redex is the inner `(square 3)`. This
yields:

```
@
| \
|  \
|   @---3
|   |\  |
|   | \ |
|   |  @
@---    \
|        \
|         *
*
```

The only redex is now the inner multiplication, so we reduce that.

```
@
| \
|  \
|   9
|   |
|   |
|   |
@---
|
|
*
```

And now we can reduce the outer multiplication, which directly yields `81`.

So the general steps are:

1. Find the next redex
2. Reduce it
3. Update the root of the redex with the result

Unwinding the spine
-------------------

To find the next redex, we first need to find the outermost function application. To do
that, we follow these steps:

1. From the root of the graph, follow the left branch until you reach a supercombinator or
primitive.
2. Check how many arguments the supercombinator or primitive takes and go back up the
graph that number of times - you're now at the root of the outermost function application.

The chain of left-branching application nodes is called the _spine_, and the process of
traversing it like this is called known as _unwinding_ the spine. The function arguments
are typically stored on a stack to make them easy to access.

If the function is a supercombinator, then we've found a redex. If it's a primitive, then
we may first need to reduce the arguments to the function before the application becomes
reducible.

If we need to reduce an argument, we must put the current stack to one side and begin
unwinding again from the root of the argument. We may need to repeat this if the argument
contains further unevaluated function applications. To keep track of these stacks we use
a stack of stacks, known as a _dump_. When we need to evaluate an argument we push the
current stack on to the dump, and when we've finished evaluating it we pop the old stack
off the dump.

TODO: graphical example

Reducing supercombinator applications
-------------------------------------

A supercombinator is reduced by substituting arguments into its body. If there are
`let(rec)` expressions in the body, they are represented as paths in the graph. For
example:

```
let y = 3
 in + y y
```

is represented as

```
@
| \
|  \
@---3
|
|
+
```

The `let` expression defines a sub-expression `3`, which is named `y`. The body of the
`let` expression references `y` via pointers to `3`. In this way, the single instance of
`y` is shared between the two arguments to `+`.

Let's look at a more complex example: the reduction of a supercombinator application.

```
f x = let y = * x x
       in + y y
main = f 3
```

We start with the `main` supercombinator:

```
main
```
which reduces to
```
@
| \
|  \
f   3
```
which reduces to
```
@
| \
|  \
@---@[y]
|   | \
|   |  \
+   @---3
    |
    |
    *
```
We can see that both arguments to `+` point to the same sub-expression (labelled `y`) and
both arguments to `*` point to the same instance of `3`. The outermost application is that
of `+`, but it requires both of its arguments to be evaluated first. Both arguments are
the inner application of `*`, which we can reduce.
```
@
| \
|  \
@---9
|
|
+
```
which reduces to
```
18
```

Lazy Evaluation
---------------

When reducing an application, there are two things we must consider:

- The argument may contain redexes, so we want to avoid copying it.
- The redex may be shared, so we want to update it with its result after reduction.

Indirection nodes: TODO
Redex updates: TODO

[intro]: 2019-03-02-implementing-a-functional-language.html

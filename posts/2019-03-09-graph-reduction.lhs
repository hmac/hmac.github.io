---
title: "Implementing A Functional Language Part II: Graph Reduction"
publish: true
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
We can model it as a graph[^1] of nested applications. For example, the expression `f 1 2`
looks like this:

```tree
digraph {
a [ label = "@" ]
b [ label = "@" ]

a -> b
b -> f
b -> 1
a -> 2
}
```

We use the symbol `@` to denote an application node. Note that though `f` takes two
arguments, we apply each in turn via currying. Here's a more complicated example - the
graph of `(+ (* 2 3) 4)`:

```tree
digraph {
a [ label = "@" ]
b [ label = "@" ]
d [ label = "@" ]

a -> b
b -> "+"
a -> 4

c [ label = "@" ]
b -> c
c -> d
c -> 3
d -> "*"
d -> 2
}
```

To evaluate the program, we'll repeatedly _reduce_ expressions in the graph. We'll stop
when there are no expressions left to reduce. The resulting graph is the result of the
program. Reducing an expression typically involves applying a function to one or more
arguments, producing a result, and then replacing the expression with the result.

Not all expressions can be reduced: `+ 1 2` reduces to `3` but `3` does not reduce any
further. Similarly, `+ 1` does not reduce because `+` requires two arguments. An
expression which is reducible is known as a reducible expression, or redex. To evaluate an
entire program, all we need to do is repeatedly identify the next redex to reduce, and
then reduce it, stopping when there are no redexes left.

Let's try to reduce the graph above. We start at the top, with an application node `@`.
The child nodes are `@` (another application) and `4`. We can't immediately evaluate this
expression, so we proceed down into the child application node, which itself has children
`+` and `@`. `+` takes two arguments, in this case `(* 2 3)` and `4`. `+` also requires
that both its arguments are evaluated, so we need to continue to reduce the first
argument. We descend into the right hand application node - the root of `(* 2 3)`. Here's
where we are in the graph:

```tree
digraph {
a [ label = "@" ]
b [ label = "@" ]
d [ label = "@" ]


a -> b
b -> "+"
a -> 4

c [ label = "@", color = red ]
b -> c
c -> d
c -> 3
d -> "*"
d -> 2
}
```

As before, we descend into the child application node, finally reaching `*`. `*` takes two
numeric arguments, and we have two arguments in `2` and `3`. We've found a reducible
expression! Specifically, the subgraph

```tree
digraph {
a [ label = "@", color = red ]
b [ label = "@" ]


a -> b
a -> 3
b -> "*"
b -> 2
}
```

can be reduced to the single numeric node `6`. The node marked in red is the _root_ of
the redex. Performing the reduction, the graph now looks like this:

```tree
digraph {
a [ label = "@" ]
b [ label = "@" ]


a -> b
a -> 4
b -> "+"
c [ label = "6", color = red ]
b -> c
}
```

Notice that the expression `(* 2 3)` has been replaced with the result of its reduction
(`6`). Proceeding back up graph, we can see that the arguments to `+` are now both
evaluated, so we've found another redex. Reducing this, we end up with a single node `10`,
which is the result of the whole expression.

Reduction order
---------------

The order in which we reduce expressions in a program has a profound effect on the
behaviour of the program. The approach we used in reducing the program above is called
_normal order reduction_. Normal order reduction requires that we reduce the leftmost
outermost redex first.

One consequence of this is that we reduce applications before reducing their arguments.
This is in contrast to a typical imperative language, where arguments to functions are
evaluated before the function is called. Take for example the following Core program:
```
K x y = x
main = K 1 (/ 1 0)
```
where we can assume that `(/ 1 0)` will raise an error if evaluated. Following normal
order reduction, we will reduce the application of `K` first, resulting in the following:
```
main = 1
```

Since the second argument to `K` is never used, we never evaluate it. In a typical
imperative language, we would evaluate `(/ 1 0)` first, resulting in an error. This style
of execution is called _lazy evaluation_, in contrast to _strict evaluation_. The two key
properties of lazy evaluation are the following:

- Arguments to functions are only evaluated when needed.
- Each argument is only evaluated once - further uses of the argument will use the result
  from the initial evaluation.

The second property doesn't affect program behaviour but greatly improves efficiency.

Normal form and Weak head normal form
-------------------------------------

Normal order reduction specifies that we reduce the leftmost outermost redex first,
but it doesn't specify when to _stop_. A natural assumption is to stop when there are no
redexes left, but this is not our only option. If the output of our program is being
printed to the screen, and we want to show progress as we go (or we're printing an
infinite stream of values) then we want to be able to produce output without having fulling
evaluated it yet. Imagine a list made from a series of `Cons` cells linked together: we
may want to print the first element before having evaluated the whole list.

To do this, we need to stop reducing when there is no longer a _top-level_ redex. This
will allow us to inspect the structure and decide what to evaluate next. An expression
which has no top-level redexes (but may have inner redexes left) is in _weak head normal
form_ (WHNF). An expression which has no redexes at all is in _normal form_ (NF). All
expressions in NF are also in WHNF, but not vice versa. Here are some examples.

Normal Form    Weak Head Normal Form
-----------    -------------
`3`            `3`                                   
`(+ 1)`        `(+ 1)`                                   
               `(+ (+ 2 3))`                                   
-----------    -------------

For Core we'll follow lazy evaluation and evaluate expressions to WHNF. For some built in
functions (like `+`) we will adopt strict semantics, requiring arguments to be fully
evaluated to normal form. For all user-defined functions we'll use lazy semantics.

Reducing programs
-----------------

Let's walk through the evaluation of the following program:
```
square x = * x x ;
main = square (square 3)
```

To start with, the graph of our program consists of just the node `main`. For reference
we've also drawn the graph of the supercombinator `square`, though we don't evaluate this
until it appears in `main`.

```tree
digraph {
main
}
```

```graph
digraph {
label = "square"
a [ label = "@" ]
b [ label = "@" ]
a -> b

c [ label = "@" ]
b -> c

a -> x
b -> x

c -> "*"
}
```

`main` is a
supercombinator with no arguments, so it is a redex. We reduce it by replacing it with its
body, yielding the following graph:

```tree
digraph {
a [ label = "@", color = "red" ]
square1 [ label = "square" ]
a -> square1

b [ label = "@" ]
a -> b

square2 [ label = "square" ]
b -> square2
b -> "3"
}
```

The outermost redex is the top node - the outer application of `square`. To reduce it, we
replace the redex with an instance of the function body, substituting any parameters with
a pointer to the argument of the application. This gives us:

```graph
digraph {
a [ label = "@", color = "blue" ]
b [ label = "@", color = "blue" ]
star [ label = "*", color = "blue" ]
a -> b [ color = "blue" ]

b -> star [ color = "blue" ]
b -> c [ color = "blue" ]

c [ label = "@" ]
a -> c [ color = "blue" ]
c -> "square"
c -> "3"

}
```

We see that the inner redex `(square 3)` is now shared between two application nodes.
Notice that this transformation has resulted in a true _graph_ rather than a tree. The
subgraph coloured blue is the instantiated body of `square`.

The application of `*` cannot be reduced because `*` is a strict primitive and requires
both its arguments to be evaluated first. Hence the only redex is the inner `(square 3)`.
This yields:

```graph
digraph {
a [ label = "@" ]
b [ label = "@" ]
"3" [ color = "blue" ]
a -> b

star1 [ label = "*" ]
b -> star1
c [ label = "@", color = "blue" ]
b -> c

a -> c
d [ label = "@", color = "blue" ]
star2 [ label = "*", color = "blue" ]
c -> d [ color = "blue" ]
d -> star2 [ color = "blue" ]
c -> "3" [ color = "blue" ]
d -> "3" [ color = "blue" ]
a -> "3" [ style = "invis" ]
}
```

The subgraph coloured blue is the second instantiation of `square`. The only redex is now
the inner multiplication, so we reduce that.

 ```graph
 digraph {
 a [ label = "@" ]
 b [ label = "@" ]

 a -> b
 b -> "*"

 nine [ label = "9" ]
 a -> nine
 b -> nine
 }
 ```

And now we can reduce the outer multiplication, which directly yields `81`.

So the general steps are:

1. Find the next redex
2. Reduce it
3. Update the root of the redex with the result

Unwinding the spine
-------------------

To find the next redex, we need to find the leftmost outermost function application. To do
that we follow these steps:

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

Reducing `let(rec)` expressions
-------------------------------

A supercombinator is reduced by substituting arguments into its body. If there are
`let(rec)` expressions in the body, they are represented as paths in the graph. For
example:

```
let y = 3
 in + y y
```

is represented as

```graph
digraph {
a [ label = "@" ]
b [ label = "@" ]

a -> b
b -> "+"
b -> "3"

a -> "3"
}
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

```graph
digraph { main }
```
which reduces to
```graph
digraph {
a [ label = "@" ]
a -> f
a -> "3"
}
```
which reduces to
```graph
digraph {
node [ fontname = "courier" ]
a [ label = "@" ]
b [ label = "@" ]

a -> b
b -> "+"

c [ label = "@ (y)" ]
a -> c
b -> c

d [ label = "@" ]
c -> d
d -> "*"
c -> "3"
d -> "3"
}
```
We can see that both arguments to `+` point to the same sub-expression (labelled `y`) and
both arguments to `*` point to the same instance of `3`. The outermost application is that
of `+`, but it requires both of its arguments to be evaluated first. Both arguments are
the inner application of `*`, which we can reduce.
```graph
digraph {
node [ fontname = "courier" ]
a [ label = "@" ]
b [ label = "@" ]

a -> b
b -> "+"
a -> "9"
b -> "9"
}
```
which reduces to
```graph
digraph {
18
}
```

Reducing supercombinator applications
-------------------------------------

When reducing an application, there are two things we must consider:

- The argument may contain redexes, so we want to avoid copying it.
- The redex may be shared, so we want to update it with its result after reduction.

To do this we construct a new instance of the supercombinator body, substituting a
_pointer_ to the argument in place of the function parameter. This avoids having to copy
the argument, which may be large. Once we've reduced a redex, we overwrite the root of the
redex with the result of the reduction. This ensures that any other references to it will
not have to reduce it again.

There's one case we need to be careful of, however. Consider the following Core program:
```
id x = x
main = let y = 4
        in * (id y) y
```

After some reduction, the graph of this program looks like this:
```graph
digraph {
a1 [ label = "@" ]
a2 [ label = "@" ]
a1 -> a2


a1 -> 4

a2 -> "*"
y [ label = "@" ]
y -> "id"
y -> 4
a2 -> y

}
```

Let's consider the reduction of `(id 4)`. If we were to naively update the root of the
redex with the result, we'd end up with the following.

```graph
digraph {
a1 [ label = "@" ]
a2 [ label = "@" ]
a1 -> a2


a1 -> 4

a2 -> "*"
z [ label = "4" ]
a2 -> z

}
```

We've duplicated `4`! If this was a large expression, we would be at risk of doing extra
work reducing it twice. To get around this, we can add a new type of node: an _indirection
node_. This node simply points to another node, and we can use it to update the root of
the redex without duplicating the result. Using an indirection node (marked by a `#`) we
get the following:

```graph
digraph {
a1 [ label = "@" ]
a2 [ label = "@" ]
a1 -> a2


a1 -> 4

a2 -> "*"
ind [ label = "#" ]
ind -> 4
a2 -> ind

}
```

Those are the basics of graph reduction. In the next section we'll apply this theory to
our first compiler: the template instantiation machine.

[^1]: In this case we could call it a tree, but trees are just a specific type of graph.
Later on we'll see that certain transformations on the tree will make it no longer a valid
tree. So for simplicity we'll refer to everything as a graph.

[intro]: 2019-03-02-implementing-a-functional-language.html

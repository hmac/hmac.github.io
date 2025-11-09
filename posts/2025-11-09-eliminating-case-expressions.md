---
title: Eliminating case expressions
publish: true
---
# Eliminating case expressions

A quick note to remind myself in future: case expressions can be implemented quite nicely via a combination of eliminators and CPS.

In [tl](https://github.com/hmac/tl) I have a small functional language which compiles to a custom instruction sequence for a simple stack machine.
All instructions are simple (and have analogues in real assembly languages) except for `case`, which is used to implement case expressions.

```
Case(Vec<(Pattern, usize)>)
```

The `case` instruction contains a map of patterns to relative jumps. The patterns themselves are complex nested objects representing the source level patterns in tl.
For example here is a tl function containing two case expression:

```
firstByOpt : (a -> opt.Option b) -> List a -> opt.Option (a, b) {
  f xs -> case xs {
    [] -> opt.None,
    [x, ..z] -> case f x {
      opt.Some r -> opt.Some (x, r),
      opt.None -> firstByOpt f z
    }
  }
}
```

`[]` matches an empty list. `[x, ..z]` matches a list with at least one element (bound to `x`) and binds the tail of the list to `z`.
`opt.Some r` matches the constructor `Some`, binding its one argument to `r`. `opt.None` matches the constructor `None`, which has no arguments.

Letting this complexity leak down into the VM has three drawbacks:
- The VM has a complex, recursive `match_pattern` routine which does pattern matching at runtime
- The `case` instruction doesn't map well onto any instruction in common targets such as x86, ARM or WASM.
- There's something aesthetically unpleasant about it

It's well known that you can compile case expressions into decision trees, for example in [this paper](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=aa003e97e38af183aa7bd1fbb2d615d5ae3f57f0). But I'm not concerned yet about an optimal representation, I'm happy to just match each pattern in order.

After a bit of thinking I realised that there's a nice representation that only requires we add one thing to the language: data type eliminators.
For each type we generate an eliminator, which is a function that can break down a value and give you its constituent parts. Best explained with examples.

The eliminator for `Bool`:
```
type Bool { True, False }
elim_Bool : a -> a -> Bool -> a

type Option a { Some a, None }
elim_Option : (a -> b) -> b -> Option a -> b

type Result a b { Ok a, Err b }
elim_Result : (a -> c) -> (b -> c) -> Result a b -> c

type List a { Cons a (List a), Nil }
elim_List : (a -> List a -> b) -> b -> List a -> b
```

You might notice that the eliminator for `Bool` is basically an `if` statement with the arguments swapped around a bit. The eliminator for `List` is just `fold`.
There's a simple scheme for generating these: 1 function arg for each constructor, each function takes an arg for each constructor arg, then a final arg for the value.

We can take any case branch from `firstByOpt` above and rewrite it using an eliminator:

```
// [] -> opt.None
xs -> elim_List (_ _ -> <fail>) opt.None xs

// [x, ..z] -> ...
xs -> elim_List (x z -> ...) <fail> xs
```

So each branch becomes a function that takes the target as argument and returns its right hand side, or `<fail>` if the target doesn't match the pattern.

Eliminators give us a way to branch on the constructor and also bind its arguments to variables. This is the essence of a case expression.
The remaining bit is to represent matching case branches, in order. We can do this in continuation-passing style: to each branch function we pass the _next_
branch to try. This also gets rid of the special `<fail>` value at the same time:

```
case xs {
  [] -> opt.None,
  [x, ..z] -> rest_of_program
}


// becomes

( next xs -> elim_List opt.None (_ _ -> next xs) xs )
( ( next xs -> elim_List next (x z -> rest_of_program) xs )
  ( <panic> ) // no branches left!
)
xs
```

There are two tricky things remaining. One is the special `<panic>` builtin, which is only really necessary to make sure the program stops rather than carrying on
and executing some random code. The typechecker should already check for case exhaustiveness (though this doesn't fully work yet).
The other tricky thing is that we don't really want to evaluate the right-hand side of case branches unless the target matches their pattern.
tl is strict and pure so this doesn't affect the result, but it is wasteful.
Possibly we adjust the eliminators so that each function argument takes an additional argument which is the Unit type, thereby preventing them from being evaluated eagerly.

Another thing we could do is inline/partially evaluate the CPS expression, so it would become

```
xs -> elim_List (_unit -> opt.None) (_ _ _unit -> elim_List (_unit -> <panic>) (x z _unit -> rest_of_program)) xs) xs
```

but if the data type has many constructors and there are many branches, this would cause the expression to balloon in size.

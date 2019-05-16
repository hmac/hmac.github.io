---
title: "Evaluating the Untyped Lambda Calculus"
publish: true
---

Evaluating the Untyped Lamda Calculus
=====================================

Recently I was working through some problems related to the Untyped Lambda Calculus and
was getting tired of reducing expressions by hand. I tried writing a small evaluator to
help, and was surprised how straightforward it turned out to be. The evaluator is only 40
lines or so - easily small enough to fit in a blog post. So here it is! The source for
this post is [here][source].

> module Lambda where

> import Data.Maybe (fromMaybe)

The first thing we need is a representation for lambda terms. The lambda calculus is very
simple: we have lambda abstraction (i.e. functions), variables, and applications.

> data Expr = Lam String Expr -- \x. e
>           | Var String      -- x
>           | App Expr Expr   -- f x
>           deriving (Eq, Show)

Thus the Church numeral one `\f. \x. f x` becomes:
```
Lam "f" (Lam "x" (App (Var "f") (Var "x")))
```
We use `String` for variable bindings, for simplicity.

What we want by the end is a function `nf` which computes the normal form of a given
lambda expression - that is, the result when we reduce it as much as possibe. `Context`
contains the free variables in scope for a given expression. It's simply a map from
variable name to the expression bound to that variable. When evaluating a full expression
we'll typically start with a empty context (`mempty`).

> nf :: Context -> Expr -> Expr
> nf ctx e = head (reduceList ctx e)

`reduceList` produces a list of consecutive reductions of the expression, with the last
one at the front. To compute the normal form we just take this first element.

> type Context = [(String, Expr)]
>
> reduceList :: Context -> Expr -> [Expr]
> reduceList ctx expr = go [expr]
>   where go (e : es) = let e' = reduce ctx e
>                        in if e' == e
>                           then (e : es)
>                           else go (e' : e : es)

You can see that we build up a list of reduced expressions, stopping when reducing the
expression no longer changes it. If the expression does not reduce to normal form (e.g. Ω)
then this function will not terminate.

The actual work happens in `reduce`:

> reduce :: Context -> Expr -> Expr
> reduce ctx (Var v) = fromMaybe (Var v) (lookup v ctx)
> reduce ctx (Lam v a) = Lam v (reduce ctx a)
> reduce ctx (App (Lam v a) b) = substitute v a b
> reduce ctx (App a b) = App (reduce ctx a) (reduce ctx b)

`reduce` applies one round of beta reduction to the expression. Symbolically, beta
reduction is defined as `(\x. e) a ↝ e[a/x]`. This means that when we have an application
of a lambda abstraction with a variable `x` over an expression `e` to another lambda
expression `a`, we can reduce it to `e` with all instances of `x` replaced by `a`. The
only thing we need to be sure of is that `x` does not appear as a free variable in `a`.

So what `reduce` does depends on the type of expression:

- If it's a variable, we look up its value in our context and replace it with the value.
- If its a function, we reduce the inner expression it contains.
- If it's an application of a function to another expression, we substitute every
  occurrence of the variable bound by the function with the expression being applied.
- For any other application, we just reduce both inner expressions.

The final piece we need is `substitute`, which performs the variable substitution.

> substitute :: String -> Expr -> Expr -> Expr
> substitute v a b = go a
>   where go (Var v')   | v' == v = b
>                       | otherwise = Var v'
>         go (Lam v' e) | v' == v = Lam v' e
>                       | otherwise = Lam v' (go e)
>         go (App x y) = App (go x) (go y)

`substitute` replaces all instances of a variable with an expression. To avoid variable
capture, it won't recurse inside a lambda abstraction of the same variable name.

Some examples:
```
substitute "x" (Var "x") e == e
substitute "x" (App (Var "f") (Var "x")) e == App (Var "f") e
substitute "x" (Lam "y" (Var "x")) e == Lam "y" e
substitute "x" (App (Var "x") (Lam "x" (Var "x"))) e == App e (Lam "x" (Var "x"))
```

That's it! We can now define and reduce some lambda expressions.

```
> nf mempty (App (Lam "x" (Var "x")) (Var "y"))
Var "y"
> nf mempty (App (App (Lam "f" (Lam "x" (App (Var "f") (Var "x")))) (Lam "e" (Var "e"))) (Var "t"))
Var "t"
```

These aren't very nice to write by hand, but parsing untyped lambda calculus is quite
straightforward. I might tackle that in a future blog post.

Since writing this evaluator I've been experimenting with the other variants of lambda
calculus, in particular the different type systems T, F etc. I'll probably write at least
another blog post on what I've learned but until then the experiments are [on
GitHub](https://github.com/hmac/lc). There's also an online version which you can use to
play around with the different lambda calculi [here](https://hmac.dev/lc/index.html).

[source]: https://github.com/hmac/hmac.github.io/blob/src/posts/2019-05-15-evaluating-lambda-calculus.lhs

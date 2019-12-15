---
title: Recursion with Context
publish: true
---
# Recursion with Context

Whilst writing a source code formatter for [Lam](https://github.com/hmac/lam) I
stumbled into the problem of when to add parentheses to expressions. I was
printing Haskell style type signatures, which sometimes need parentheses to
indicate precedence. For example, `f a b` is interpreted as `f(a, b)` whereas `f
(a b)` is interpreted as `f(a(b))`.

When printing a type signature you want to add just enough parentheses that the
output is unambiguous, but no more than that. This turned out to be tricky! I
tried several approaches, each revealing edge cases I hadn't thought of, until I
came up with a more structured approach which I'm fairly happy with. I have a
hunch this is quite a general technique that is used elsewhere, but this was the
first time I've encountered a problem that seems to be a good fit for it.

Type signatures in Lam are represented by this data type (simplified a bit):
```haskell
data Type = Type :@: Type   -- applications
          | TyArr           -- function arrows
          | TyVar String    -- type variables
```

The reason for this structure is because it fits in well with another part of
the compiler, so I didn't want to change it just to make printing easier.

Now there are two major syntactic structures in Lam type signatures that you
need to deal with: function types (e.g. `a -> b`) and type applications (e.g.
`f a`). Type applications are directly represented by the `:@:` constructor,
so `f a` is `TyVar "f" :@: TyVar "a"`. There's no dedicated constructor for
function types - instead they're represented as an application of the type
`(->)` to two other types. `a -> b` is:
```haskell
( TyArr :@: TyVar ( Name "a" ) ) :@: TyVar ( Name "b" )
```

This is a bit clunky to construct, so I have a helper function `fn` which is
used infix to generate a function type.
```haskell
infixr 4 `fn`
fn :: Ty -> Ty -> Ty
a `fn` b = (TyArr :@: a) :@: b
```

We need to add parentheses to separate nested function types and type
applications from each other. Here are some examples:
```haskell
a -> b
(a -> b) -> f a -> f b
t (f a) -> f a
(a -> f b) -> t a -> f (t b)
(a -> b) -> p r a -> p r b
```

The rules for when to add parentheses aren't immediately obvious, but after a
bit of experimentation I came up with this:

* applications and arrows at the top level don't get parenthesised
* arrows on the left of arrows get parenthesised
* arrows on the right of arrows don't get parenthesised
* arrows on either side of applications get parenthesised
* applications on the left of applications don't get parenthesised
* applications on the right of applications get parenthesised
* applications on either side of arrows don't get parenthesised

The difficulty with implementing this in a typical recursive way is that there
are five different "states" you can be in, and you need to do different things
depending on the state and the element you're looking at. So I tried capturing
the "state" explicitly as a sort of surrounding context.

```haskell
data Context = Root -- you're at the top level or can pretend you are
             | AppL -- you're on the left hand side of an application
             | AppR -- you're on the right hand side of an application
             | ArrL -- you're on the left hand side of an arrow
             | ArrR -- you're on the right hand side of an arrow
```

The idea here is that the context tells you where you are in relation to the
wider expression, so you can make decisions based on that even though you can't
"see" any of the wider expression at the time. An example should make things
clearer, so let's walk through how this is used.

To print a type, I pass it to my function along with an initial context of
`Root`[^1].
```haskell
printType :: Type -> String
printType ty = print' Root ty
```

`print'` examines the context and the type and uses the combination of the two
to determine what to do.

```haskell
print' :: Context -> Type -> String
print' ctx ty = case (ctx, ty) of
```

We start by matching the case of a function arrow. We print either side,
separated by an arrow.
```haskell
  (Root, (TyArr :@: a) :@: b) -> print' ArrL a <+> "->" <+> print' ArrR b
```
The left hand side (`a`) gets the context `ArrL` and the RHS gets `ArrR`.

We do a similar thing with applications.
```haskell
  (Root, a :@: b) -> print' AppL a <+> print' AppR b
```

That's all we need to do to ensure that the correct context is propagated
through our AST. The next eight patterns are just translations of the rules we
wrote above. We use a helper function `parens` which wraps its argument in
parentheses.

Arrows on the left of arrows get parenthesised:
```haskell
  (ArrL, (TyArr :@: a) :@: b) -> parens $ print' Root (a `fn` b)
```

Notice that in the recursive call we reset the context to `Root`, because we've
gone inside parentheses. This ensures we don't add unnecessary parentheses such
as `((a -> b)) -> a -> b`.

Arrows on the right of arrows don't get parenthesised:
```haskell
  (ArrR, (TyArr :@: a) :@: b) -> print' Root (a `fn` b)
```

Arrows on either side of applications get parenthesised:
```haskell
  (AppR, (TyArr :@: a) :@: b) -> parens $ print' Root (a `fn` b)
  (AppL, (TyArr :@: a) :@: b) -> parens $ print' Root (a `fn` b)
```

Applications on the left of applications don't get parenthesised:
```haskell
  (AppL, a :@: b) -> print' Root (a :@: b)
```

Applications on the right of applications get parenthesised:
```haskell
  (AppR, a :@: b) -> parens $ print' Root (a :@: b)
```

Applications on either side of arrows don't
```haskell
  (ArrL, a :@: b) -> print' Root (a :@: b)
  (ArrR, a :@: b) -> print' Root (a :@: b)
```

Finally we have the basic case: type variables. We don't care about the context
when printing these.

```haskell
  (_, TyVar n) -> n
```

And that's it. Quite straightforward and (compared to my earlier attempts) very
understandable!

The key idea here is to represent the surrounding context as a data type and
pass that down through your recursive calls. This lets you make decisions based
on the larger structure simply and efficiently (we traverse the AST in one
pass).

I'm certain this technique isn't new, but it's the first time I've used it and
in this case it seems like a very good fit.

[^1]: I'm using String as the return type here for simplicity but in reality
this was an abstract document type from a pretty printing library.


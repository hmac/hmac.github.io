# Recursion with context

Thought I'd share an interesting technique I used recently when writing a
printer for type signatures. The problem was when to add parentheses. Consider
these signatures:

```haskell
a -> b
(a -> b) -> f a -> f b
t (f a) -> f a
(a -> f b) -> t a -> f (t b)
(a -> b) -> Reader r a -> Reader r b
```

The general principle is to add parentheses only when necessary to resolve
ambiguity. For example `t (f a)` implies that `t` and `f` have kind `* -> *` and
`a` has kind `*`, whereas `t f a` requires that `t` has kind `* -> * -> *`.

With a bit of experimentation the rules I came up with are:

* applications and arrows at the top level don't get parenthesised
* arrows on the left of arrows get parenthesised
* arrows on the right of arrows don't get parenthesised
* arrows on either side of applications get parenthesised
* applications on the left of applications don't get parenthesised
* applications on the right of applications get parenthesised
* applications on either side of arrows don't get parenthesised

The difficulty with implementing this in a typical recursive way is that there
are five different "states" you can be in, and you need to do different things
depending on the state and the element you're looking at. After writing and
scrapping several messy and buggy versions I tried capturing the "state"
explicitly as a sort of surrounding context.

```haskell
data Context = Root -- you're at the top level or can pretend you are
             | AppL -- you're on the left hand side of an application
             | AppR -- you're on the right hand side of an application
             | ArrL -- you're on the left hand side of an arrow
             | ArrR -- you're on the right hand side of an arrow
```

My Type AST looked roughly like this:
```haskell
data Type = Type :@: Type   -- applications
          | TyArr           -- function arrows
          | TyCon String    -- type constructors
          | TyVar String    -- type variables
```

The reason for this structure is because it fits in well with another part of my
program, so I didn't want to change it just to make printing easier. You can see
that function arrows are represented as a dumb type with no arguments. The type
`a -> b` is constructed like this: 
```
( TyArr :@: TyVar ( Name "a" ) ) :@: TyVar ( Name "b" )
```
That is, an application of `TyArr` to `a`, and an application of that to `b`.

To print a type, I pass it to my function along with an initial context of
`Root`. I'm using String as the return type here for simplicity but in reality
this was an abstract document type from a pretty printing library.

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

Arrows on the right of arrows don't:
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

Finally we have the two basic cases: type constructors and type variables. We
don't care about the context when printing these.

```haskell
  (_, TyCon n) -> n
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

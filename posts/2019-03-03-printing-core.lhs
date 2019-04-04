---
title: "Implementing a Functional Language: Printing Core"
publish: true
---

Implementing a Functional Language: Printing Core
=================================================

This is an appendix to a series in implementing a functional language. The introduction is
[here][intro]. This is a literate Haskell file - you can download the source
[here][source]. To load it into GHCi and play around, you can use the following command
(you'll need the [source][part1source] for the `Core.Language` module as well):
```
stack --resolver lts-12.2          \
      ghci --package prettyprinter \
           --package text          \
      2019-03-03-printing-core.lhs \
      2019-03-03-the-core-language.lhs
```

> {-# LANGUAGE OverloadedStrings #-}
> module Core.Print (print) where
> import Core.Language
> import Data.Text.Prettyprint.Doc
> import Prelude hiding (print)

This module exports a single function: `print`. `print` pretty prints a `CoreProgram`,
producing a `Doc a` which can be converted to a final output by the caller. `Doc` is a
type provided by the `Prettyprint` package.

We print a `CoreProgram` by printing each of its supercombinator definitions, separating
each with a newline.

> print :: CoreProgram -> Doc a
> print = (vsep . map pScDefn)

We print supercombinator by printing its name and binders on one line, and printing the
expression body across the rest of the line and possibly further lines below. The body
will be indented to preserve the off-side rule.

> pScDefn :: CoreScDefn -> Doc a
> pScDefn (name, binders, expr)
>   = pretty name <+> sepmap binders <+> equals <+> align (pExpr expr)

We print a Core expression by case analysis on its constructors.

> pExpr :: CoreExpr -> Doc a
> pExpr (EVar v) = pretty v
> pExpr (ENum n) = pretty n
> pExpr (EConstr tag arity) = "Pack{" <> pretty tag <> "," <> pretty arity <> "}"
> pExpr (EAp e1 e2) = pAExpr e1 <+> pAExpr e2
> pExpr (ELet Recursive defns expr)
>  = "letrec" <+> nest 2 (pDefns defns) <+> "in" <+> pExpr expr
> pExpr (ELet NonRecursive defns expr)
>  = "let" <+> nest 2 (pDefns defns) <+> "in" <+> pExpr expr
> pExpr (ECase e alters)
>   = "case" <+> pExpr e <+> "of" <+> hardline
>            <+> (align . vsep . punctuate semi) (map pAlter alters)
>     where pAlter (tag, binders, expr)
>             = sep $ angles (pretty tag) : map pretty binders ++ ["->", pExpr expr]

> pDefns :: [(Name, Expr Name)] -> Doc a
> pDefns defns
>   = vsep $ punctuate semi (map pDefn defns)
>     where pDefn (name, expr) = pretty name <+> equals <+> align (pExpr expr)

`pAExpr` is like `pExpr`, but wraps the result in parentheses unless it is a variable or
number. This is naive but avoids ambiguity.

> pAExpr :: CoreExpr -> Doc a
> pAExpr (EVar v) = pretty v
> pAExpr (ENum n) = pretty n
> pAExpr e = parens (pExpr e)


`sepmap` is a convenience function for printing each element in a list separated by spaces.

> sepmap :: Pretty a => [a] -> Doc b
> sepmap xs = sep (map pretty xs)

This is the entire pretty printer. Thanks to the fantastic `prettyprinter` library, it's
short and quite readable.

[intro]: 2019-03-02-implementing-a-functional-language.html
[source]: https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-03-printing-core.lhs
[part1source]: https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-03-the-core-language.lhs

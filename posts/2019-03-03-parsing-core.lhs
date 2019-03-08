---
title: "Implementing a Functional Language: Parsing Core"
---

Implementing a Functional Language: Parsing Core
================================================

This is an appendix to a series in implementing a functional language. The introduction is
[here][intro]. This is a literate Haskell file - you can download the source
[here][source]. To load it into GHCi and play around, you can use the following command
(you'll need the [source][part1source] for the `Core.Language` module as well):
```
stack --resolver lts-12.2         \
      ghci --package megaparsec   \
           --package text         \
      2019-03-03-parsing-core.lhs \
      2019-03-03-the-core-language.lhs
```

> {-# LANGUAGE OverloadedStrings #-}
> module Core.Parse (parser) where
> import Core.Language
> import Text.Megaparsec
> import qualified Text.Megaparsec.Char.Lexer as L
> import Text.Megaparsec.Char
> import Data.Text (Text)
> import qualified Data.Text as T
> import Data.Void
> import Data.Proxy
> import Control.Monad (guard)

This module exports one function: `parser`, which is the parser for our language. We can
pass it to functions like `Text.Megaparsec.Parse` to parse input into a
`Core.CoreProgram`.

Our parser is formed by combining together smaller parsers, in typical parser combinator
style. We put very little effort into good error messages, in the interests of brevity.
Here we define a type for all our parsers, which specifies a basic error type and an input
type of Text. See the Megaparsec documentation for more information on this.

> type Parser = Parsec (ErrorFancy Void) Text

Lexing
------

Lexing is the process of converting a stream of characters to a stream of tokens. It's
typically used to strip out whitespace, comments, and other parts of the input that have
no bearing on the parse result. To avoid having to process the input twice, we intersperse
lexing with parsing by wrapping all primitive parsers in a _space consumer_ which is
responsible for stripping any trailing "space".

> spaceConsumer :: Parser ()
> spaceConsumer = L.space space1 (L.skipLineComment "--") empty

> lexeme :: Parser a -> Parser a
> lexeme = L.lexeme spaceConsumer 

> symbol = L.symbol spaceConsumer

> semicolon :: Parser Text
> semicolon = symbol ";"

> parens :: Parser a -> Parser a
> parens = between (symbol "(") (symbol ")")

> angles :: Parser a -> Parser a
> angles = between (symbol "<") (symbol ">")

> num :: Parser Int
> num = lexeme L.decimal

> identifier :: Parser Text
> identifier = lexeme $ do
>   first <- letterChar
>   rest <- many alphaNumChar
>   pure $ tokensToChunk (Proxy :: Proxy Text) (first : rest)

We build in support for Core's operators and keywords.

> operators :: [Parser Text]
> operators = map symbol ["==", "!=", ">=", "<=", "+", "-", "*", "/", ">", "<"]

> keywords :: [Text]
> keywords = ["let", "letrec", "case", "in", "of", "Pack", "->"]

Parsing
-------

A Core program is a collection of supercombinator definitions.

> parser :: Parser CoreProgram
> parser = supercombinator `sepBy1` semicolon

A supercombinator is a name, an optional list of arguments, and a expression representing
the function body.

> supercombinator :: Parser CoreScDefn
> supercombinator = do
>   name <- var
>   args <- many var
>   symbol "="
>   body <- expr
>   pure (name, args, body)

A variable name must start with an alphanumeric character and cannot be one of the
built-in keywords.

> var :: Parser Name
> var = choice $ operators ++ [try alphaVar]
>       where alphaVar = do
>               v <- identifier
>               if v `elem` keywords
>               then
>                 fail . T.unpack $ "cannot use keyword " <> v <> " as variable"
>               else
>                 pure v

An expression is either a let(rec), an application, a constructor, a numeric literal or a
variable. To avoid the problem of left recursion we handle applications separately from
simple expressions. If we didn't do this, parsing "f x" would recurse infinitely on 'f'.

> expr :: Parser CoreExpr
> expr = let_ <|> case_ <|> application <|> aexpr

> aexpr :: Parser CoreExpr
> aexpr = parens expr <|> constructor <|> fmap ENum num <|> fmap EVar var

> application :: Parser CoreExpr
> application = foldl1 EAp <$> some aexpr

> let_ :: Parser CoreExpr
> let_ = do
>   isRec <- letrec
>   defns <- letDefn `sepBy1` semicolon
>   symbol "in"
>   e <- expr
>   pure $ ELet isRec defns e
>     where letrec = (symbol "letrec" >> pure Recursive) <|> (symbol "let" >> pure NonRecursive)
>           letDefn = do
>             v <- var
>             symbol "="
>             e <- expr
>             pure (v, e)
>   
> case_ :: Parser CoreExpr
> case_ = do
>   symbol "case"
>   scrutinee <- expr
>   symbol "of"
>   alts <- caseAlternatives
>   pure $ ECase scrutinee alts

> caseAlternatives :: Parser [Alter Name]
> caseAlternatives = alt `sepBy1` semicolon
>   where alt = do
>           tag <- angles num
>           args <- many var
>           symbol "->"
>           e <- expr
>           pure (tag, args, e)

> constructor :: Parser CoreExpr
> constructor = do
>   symbol "Pack{"
>   tag <- num
>   symbol ","
>   arity <- num
>   pure $ EConstr tag arity

This is all there is to the parser. Like the printer, it is short and sweet.


[intro]: 2019-03-02-implementing-a-functional-language.html
[source]: https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-03-parsing-core.lhs
[part1source]: https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-03-the-core-language.lhs

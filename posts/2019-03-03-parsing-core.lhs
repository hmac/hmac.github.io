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

> module Core.Parse (parser, parseTest) where
> import Core.Language
> import Text.Megaparsec

This module exports two functions: `parser` and `parseTest`. `parser` is the parser for
our language. We can pass it to functions like `Text.Megaparsec.Parse` to parse input into
a `Core.CoreProgram`. `parseTest` is a convenience function that will take a `String` as input and
produce a `Core.CoreProgram`, throwing an error if the parse fails.

> parser = error "Not implemented yet!"

> parseTest = error "Not implemented yet!"

[intro]: 2019-03-02-implementing-a-functional-language.html
[source]: https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-03-parsing-core.lhs
[part1source]: https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-03-the-core-language.lhs

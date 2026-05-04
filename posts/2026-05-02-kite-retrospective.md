---
title: A Retrospective on Kite
publish: true
---

# A Retrospective on Kite

A couple of years ago I started working on a new programming language called Kite. The driving idea was: what would Haskell look like if it were invented today? I had spent the previous four years learning Haskell in my spare time and working on a large, mature Ruby codebase at my day job. It was clear to me that Haskell would solve two big problems we were struggling with at work:

* Low confidence in code correctness despite writing huge quantities of unit tests
* No good tools for enforcing modularisation 

The second problem was becoming more and more important as the company grew and we formed new teams that were focused on particular product areas. Teams wanted to own their section of the codebase, but as a big Rails app everything was highly interconnected and we had few mechanisms for enforcing interfaces between parts of the code. ActiveRecord, Rails’ expressive ORM, was always globally available and made it easy to dig into any combination of database tables from anywhere in the application. If we wanted to, say, split a coherent set of tables into a separate database, we had no idea what parts of the application would break as a result. Even if we could define a neat boundary around a logical component, there was nothing to stop someone breaking it tomorrow. This led to some wacky approaches such as writing new code in a separate Rails “engine” that would be loaded into the main app on boot. Ultimately and inevitably we started to split functionality into separate services and communicate over RPC.

In Haskell I saw a way to tackle these problems that didn’t involve throwing the baby out with the bathwater. A combination of Haskell’s module system and its separation of pure and effectual code would allow us to isolate database access behind a single interface, and from there we could start to draw boundaries that would stick. The same set of features would allow us to test our business logic with a mocked database, potentially drastically speeding up our test suite. Our internal coding style was already quite functional, often adopting a sort of single-static-assignment style that would fit in well with Haskell’s functional paradigm.

But despite these attractive possibilities, I could not imagine trying to pitch Haskell to my colleagues. Even as someone who had many years of experience using it, I struggled to find a combination of build tool, compiler configuration, libraries and application structure that did not have serious drawbacks. Haskell as a language and ecosystem has so many pitfalls, many of which you only discover by falling into them. String vs Text, record field naming, exceptions, Template Haskell, the entire cabal build tool… the list goes on. I don’t entirely subscribe to the view that Haskell documentation is universally poor or absent - the nature of the type system means that function signatures communicate far more than their equivalent in other languages - but there is often a lack of high level documentation to answer questions such as “what is this library for?” and “when should I use library X vs library Y?”. In order to present a smooth path for adoption, I imagined having to run ahead and explore all these options in advance, in order to compile a set of complementary libraries that suited our situation.

My journey into the world of typed functional programming eventually led me to quit my job and undertake a masters degree in Type Theory, which is the logical foundation of dependently typed programming languages. This was a whole experience in itself, and I came away with an appreciation not only for the power and elegance of Type Theory as a computational model, but also for the challenges that lay ahead on the path to mainstream adoption for dependent types. I believed that dependent types might be the future of programming, but that future is some way off. I wanted a tool that could solve problems now, not in 10 years’ time.

Kite was therefore a fundamentally conservative effort: take the parts of Haskell with the highest power-to-weight ratio and construct around them a modern ecosystem of tools that work well together.

## Many type checkers

I started by writing a tour of the language as if it already existed, describing each feature. I remember writing most of it in one go on my phone in the airport, while queuing for a flight. The initial design was based syntactically on Haskell 98. It had the same algebraic data types, functions, type classes and module system. It also had a bunch of extra features bolted on: string interpolation syntax, extensible records, totality checking, “safety” checking (more on that later), and even arbitrary YAML metadata at the top of source files. It was intended to compile to multiple platforms, including JS and Go.

The spec written, I started by writing a parser and pretty-printer for the language. One of the goals was to have built-in code formatting, so each syntax feature needed to be parseable and also printable. I implemented all the syntactic features up-front, which was a fair bit of work. In retrospect I think this is the wrong approach to starting a language, and that you are better off implementing the narrowest vertical slice of your language, from parsing to code generation, and then gradually widening the slice one feature at a time.

By far the largest amount of time was spent on the type checker. I had implemented various small type checkers in the past, for a variety of lambda calculus variants, but I had little experience with Hindley-Milner type inference or with complex language constructs such as pattern matching. Over the course of a year I wrote three versions of the type checker from scratch. This was a great learning experience but burned a huge amount of time and effort. I started by copying the type checker in the paper _Typing Haskell in Haskell_ [^0]. This was a fairly simple typechecker for Haskell 98 that had no support for higher rank types, a feature I was sure I would want. 

The next version was based on _OutsideIn(X) - Modular type inference with local assumptions_ [^1]. This paper describes the algorithm used by GHC, which has to deal with type families and all other sorts of complexity in modern Haskell. It uses the “French approach”, which walks each function generating constraints about the types involved, and then collects the constraints together and solves them in one go. This has the advantage that complex syntactic structures can be easily desugared to a series of constraints, and then the main complexity of the type checker is in the constraint solving. The downside is that most type errors arise from a failure to solve a constraint, and at this point it is hard to trace back to the offending expression in the program. This type checker was effective and fairly easy to extend, but the errors it produced were impossible to understand unless you had written the type checker yourself. 

The third version was based on the paper _Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism_ [^2]. In contrast to constraint solving, Bidirectional typechecking produces type errors as you walk the syntax tree, so it is easy to tie them to the offending expression in the program. The downside is that the machinery for more complex analysis must be carried around with you as you walk, which can easily lead to very complex code. In the case of this paper, support for higher rank types is implemented via a subtle manipulation of typing contexts that is very easy to get wrong. When I wanted to add extra features such as implicit function arguments, I struggled to fit these in to the bidirectional style where each expression must be traversed in a particular order. With the French style, it is much simpler to generate a big bag of constraints and then let the solver figure it all out. If there’s a way to combine the good error messages of the bidirectional approach with the global nature of the French approach, I would love to know about it.

## The opposite of scope creep

Over time I dropped or delayed features from Kite, as I started to feel the implementation burden of each one. Type classes were the first to go, as I started to see the appeal of the value-level dictionary passing approach described in Gabriela González’s blog post [Scrap Your Type Classes](https://haskellforall.com/2012/05/scrap-your-type-classes). The basic idea is that a typeclass such as 

```
class A b where
  f :: b -> b -> b
```

…is equivalent to a record type

```
data A b =
  A { f :: b -> b -> b }
```

And indeed this is basically how typeclasses are represented at runtime. A typeclass constraint on a function can be replaced by a normal function parameter:

```
g :: A Foo => Foo -> Bar
g foo = let x = f foo …

g :: A Foo -> Foo -> Bar
g a foo = let x = f a foo …
```

Provided you have higher rank types, you can represent any typeclass like this, including ones with multiple parameters, associated types, etc. By getting rid of typeclasses you get rid of a whole set of syntax as well as a separate namespace. The result is much much simpler. The thing you really lose is the automatic solving of typeclass instances, ie if you have a “class”

```
data Functor f = F {
  fmap :: forall a b.
          (a -> b)
       -> f a
       -> f b
}
```

Then to call `fmap`, we have to explicitly pass the particular instance (value of type `Functor f`) that we want to use. Eg

```
functorList :: Functor List
functorList = …

fmap functorList (+1) [1, 2, 3]
```

This gets tedious very quickly. My solution to this was to steal a feature from Agda and Scala: implicit arguments. The idea being that you can mark a function argument as implicit and then omit it when calling the function. At each call site the compiler will try to find a single unique value in scope of the correct type, and insert that as the argument. This has the effect of solving typeclass instances in basically the same way as Haskell does it, but using a more general and hopefully less magical approach.

Implementing the proof search to power this feature turned out to be far more difficult than I expected, largely because it didn’t mesh well with the bidirectional typechecker. I also didn't implement implicit coercions, which are needed to properly support superclasses.

## Multicase

A syntactic construct that I am quite proud of in Kite is something that I’ve been calling multicase. It’s a sort of super powered version of the LambdaCase extension in Haskell, which allows Kite to not have multiple definitions for functions.

For example, the Haskell function

```
map f [] = []
map f (x:xs) = f x : map f xs
```

can be written using multicase as

```
map =
  f []     -> []
  f (x:xs) -> f x : map f xs
```

A multicase consists of a series of branches like a case, but each branch can match multiple values simultaneously. A multicase with N patterns in each branch behaves like a function with N arguments. It subsumes the usual syntax for anonymous functions, as we can write eg

```
(x y -> x)
```

instead of 

```
(\x y -> x)
```

I’ve seen beginners get confused by patterns on the left side of a binding in Haskell, so my hope is that multicase makes it clearer what we are introducing: a new name, on the left, bound to the expression on the right. Not having multiple equations for a function removes the possibility of problems like writing two functions with the same name and having them be interpreted as two equations for the same function. 

Another syntactic departure from Haskell is that I wanted to avoid infix operators. Beginners are often put off, perhaps even scared, by the liberal use of infix operators in Haskell, where the order of evaluation can be impossible to know without looking up the precedence and associativity of each operator and then resolving it all in your head. Over time we start to memorise the common cases, but it’s all too easy for a library author to introduce a bunch of new operators and then you’re back to square one. So I banned all infix operators except `.`, the function composition operator. This operator is so useful for constructing pipelines that I believe it is worth the additional complexity. However I do wonder whether it should be written in a different form, for example like the elixir operator `<|`.

A relatively novel feature in Kite’s design is the totality and safety checker. Totality checkers are common in dependently typed languages and proof assistants like Idris and Agda, where type checking and evaluation are intertwined. Functions must be total (ie guaranteed to terminate) in order for typechecking to terminate, which is important if you want your language to be usable. Even in a non-dependently typed language, totality is a nice additional guarantee that your function probably works as intended. You can be sure that it will not enter an infinite loop or panic. 

Safety checking is a feature stolen from Safe Haskell, a lesser-known Haskell extension. The idea is to automatically mark as “safe” functions which cannot harm your computer in any way. This means they can’t perform any IO, but it also means they can’t subvert the type system (eg by using a function like unsafePerformIO or coerce). The worst thing a safe function can do is burn CPU time, perhaps in an infinite loop. Of course, if the function is also total then it cannot even do that! 

If all the functions exported by a module are safe, then the module is safe. If all modules in a package are safe, then the package is safe. It is here that we can do interesting things: a safe dependency is one that cannot pose a security risk. You don’t need to audit it (or any of its dependencies) because it cannot harm you. This kind of guarantee is, I believe, the right foundation on which to build solutions to the software supply chain security crisis we increasingly find ourselves in. I’ve [written more](https://hmac.dev/posts/2022-06-08-the-ideal-package-manager) about this particular topic in the context of package management. A lot of those ideas were originally intended to be features of Kite, but I didn't get around to implementing them.

A big difference between Kite and Haskell is that Kite is strict. I chose this largely because I thought it would be simpler to implement, but it turned out to have a lot of advantages. Firstly, the evaluation order of Kite is unambiguous, which I believe is helpful for beginners. A corollary of this is that performance and memory usage should be fairly predictable. It also makes it easier to compile Kite to other languages, which are almost always strict. 

Finally, it makes it possible to not have a garbage collector. Because we know statically when a value will be evaluated, we also know when it is no longer used and can be freed. It may be possible to do the same thing for a lazy language, but it isn’t clear to me how to do this, whereas it is clear for a strict language. In particular, Koka is a strict, functional language which uses a novel form[^3] of automatic reference counting which is similar to Swift. Reference counting operations are inserted at compile time, and the language’s strict semantics and immutable data allows for clever optimisations such as performing in-place updates on unique references.

The compile target for Kite changed a lot over the years. I started with compiling to scheme, as that was easy and there were other parts of the language I wanted to work on first. Later on, I designed a simple stack-based VM language as a compile target. The hope was this could be interpreted reasonably efficiently, but also further compiled (e.g. to C) in the future.

Kite was an absurdly ambitious project that was never going to take off, but in many ways I see it as a huge success. I haven’t worked on it in years and may never come back to it, but I learned so much from the process. It reinforced my belief that you should try to do things that seem impossibly difficult and that you are not at all qualified to do.

[^0]: Jones, Mark P. "Typing haskell in haskell." Haskell workshop. Vol. 7. 1999. [PDF](https://archive.alvb.in/msc/03_infoafp/papers/2012-12-13_HoorCollege_TypingHaskellInHaskell_dk.pdf)
[^1]: Vytiniotis, Dimitrios, et al. "OutsideIn (X) Modular type inference with local assumptions." Journal of functional programming 21.4-5 (2011): 333-412. [PDF](https://lirias.kuleuven.be/retrieve/b487ef30-9621-44f0-b642-881975edaec5)
[^2]: Dunfield, Jana, and Neelakantan R. Krishnaswami. "Complete and easy bidirectional typechecking for higher-rank polymorphism." ACM SIGPLAN Notices 48.9 (2013): 429-442. [PDF](https://dl.acm.org/doi/pdf/10.1145/2544174.2500582)
[^3]: Reinking, Alex, et al. "Perceus: Garbage free reference counting with reuse." Proceedings of the 42nd ACM SIGPLAN International Conference on Programming Language Design and Implementation. 2021. [PDF](https://dl.acm.org/doi/pdf/10.1145/3453483.3454032)

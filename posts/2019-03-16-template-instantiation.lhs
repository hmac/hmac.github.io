---
title: "Implementing A Functional Language Part III: Template Instantiation"
---

Implementing a Functional Language II: Template Instantiation
=============================================================

This is part 3 of a series in implementing a functional language. The introduction is
[here][intro]. This is a literate Haskell file - you can download the source
[here](https://github.com/hmac/hmac.github.io/blob/src/posts/2019-03-16-template-instantiation.lhs).
To load it into GHCi and play around, you can use the following command:
```
stack --resolver lts-12.2                        \
      ghci --package prettyprinter               \
           --package containers                  \
           --package text                        \
           2019-03-03-the-core-language.lhs      \
           2019-03-16-template-instantiation.lhs
```

---

The state transition system
---------------------------

In part 2 we covered the theory behind lazy graph reduction. Now we're going to use it to
construct our first compiler. For this and every successive compiler, we will describe the
behavior through a _state transition system_. This allows us to be precise about how the
compiler works while abstracting us from the actual code. The state transition system will
be a sort of specification of the compiler. The book spends some time on describing state
transition systems, but they're quite intuitive so we're going to explain things as we go
along.

A state transition system consists, unsurprisingly, of an initial _state_ and a series of
_transition rules_ which describe how the state evolves. The compiler proceeds by
repeatedly firing one of the transition rules until a terminal state is reached.

Our state is a triple
```
(stack, heap, globals)
```
or `(s,h,g)` for short.

The _stack_ is a stack of addresses, each of which identifies a _node_ in the heap. These
nodes form the spine of the expression being evaluated. The notation `a : s` denotes a
stack whose top element is `a` and whose remaining elements form the stack `s`.

The _heap_ is a collection of _nodes_, indexed by their address. The notation `h[a : n]`
denotes a heap `h` in which the address `a` points to the node `n`.

The _gobals_ are a collection of addresses for each supercombinator and primitive in the
program.

A heap node can take one of the following forms:
- `NAp a b` represents the application of the node with address `a` to the node with
  address `b`.
- `NSupercomb args body` represents a supercombinator.
- `NNum n` represents the number `n`.

These forms look similar to the `Expr` data type defined for the syntax of the language,
but they're distinct. `Expr` describes the _syntactic_ structure of the language whereas
the heap nodes describe the _operational_ structure. Conversion from one to the other is a
key part of our compiler that we'll discuss later on.

Here is our first transition rule:

```
    a : s      h[a : NAp a1 a2] g
-> a1 : a : s  h                g
```

This describes how we unwind an application node onto the stack. This rule will only fire
if the address on the top of the stack points to an `NAp` node. Repeated application of
this rule will unwind the entire spine of the expression onto the stack.

Our second rule describes how to perform a supercombinator reduction:

```
   a0 : a1 : ... : an : s h[a0 : NSupercomb [x1, ..., xn] body] g
->                 ar : s h'                                    g
  where (h', ar) = instantiate body h g[x1 -> a2, ..., xn -> an]
```

`instantiate` is a function which takes as arguments the supercombinator body, the heap
and the globals, augmented with a binding of each supercombinator parameter to the
address of its argument. It returns the address of the instantiated supercombinator body,
along with a modified heap containing the address.

Note that this rule doesn't update the root of the redex - it simply replaces it with the
result. This will result in repeated and unnecessary reduction of this expression if it is
shared, as we discussed in the previous section. We'll improve on this later on.

TODO: go direct to the sharing implementation?

Implementation
--------------

With the formalities out of the way, we can start implementing the compiler.

> {-# LANGUAGE OverloadedStrings #-}
> module Core.Compiler.Template where
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Core.Language
> import Data.List (mapAccumL)
> import Data.Text (unpack)
> import Data.Text.Prettyprint.Doc

We define the type for our state transition system.

> data TiState = TiState { tiStack :: [Addr]
>                        , tiHeap :: Heap
>                        , tiGlobals :: Globals
>                        }
> type Globals = Map Name Addr

The fields match each part of the state in our transition system. We model the stack as a
linked list of addresses. The `Heap` type is a glorified map which provides functions for
looking up nodes by their address and allocating new nodes. We model globals as a map of
`Name` to `Addr`.

We also define the `Node` type.

> data Node = NAp Addr Addr
>           | NSupercomb Name     -- name
>                        [Name]   -- argument names
>                        CoreExpr -- body
>           | NNum Int

The `compile` function takes a program and creates from it the initial state of the
machine.

> compile :: CoreProgram -> TiState
> compile program
>   = TiState { tiStack = [mainAddr], tiHeap = initialHeap, tiGlobals = globals }
>     where supercombinatorDefinitions = program ++ prelude
>           (initialHeap, globals) = buildInitialHeap supercombinatorDefinitions
>           mainAddr = Map.findWithDefault (error "main is not defined") "main" globals
>
> buildInitialHeap :: [CoreScDefn] -> (Heap, Globals)
> buildInitialHeap defs = let (heap, globals) = mapAccumL allocateSc emptyHeap defs
>                          in (heap, Map.fromList globals)
>   where allocateSc :: Heap -> CoreScDefn -> (Heap, (Name, Addr))
>         allocateSc heap (name, args, body) = (heap', (name, addr))
>           where (heap', addr) = halloc heap (NSupercomb name args body)

> prelude :: [CoreScDefn]
> prelude = [
>             ("I", ["x"], EVar "x")
>           , ("K", ["x", "y"], EVar "x")
>           , ("K1", ["x", "y"], EVar "y")
>           ]

We start by collecting all our supercombinator definitions: those defined in the program
and those listed in our built-in `prelude`. We allocate each of these on the (initially
empty) heap by converting them into a `NSupercomb` and passing this to `halloc`. We thread
the heap through each iteration so that the final heap contains every supercombinator. The
globals object is just a map from each supercombinator name to the address of the node we
have allocated in the heap. The initial address on the stack is the address of `main`,
which we expect to be defined in the program.

The `eval` function takes an initial state and repeatedly runs the machine, producing a
list of each state it passes through.

> eval :: TiState -> [TiState]
> eval state = state : rest
>   where rest | final state = []
>              | otherwise = eval nextState
>         nextState = step state

> final :: TiState -> Bool
> final TiState { tiStack = [addr], tiHeap = heap } = isDataNode (hlookup heap addr)
> final TiState { tiStack = [] } = error "Empty stack"
> final _ = False

> isDataNode :: Node -> Bool
> isDataNode (NNum _) = True
> isDataNode _ = False

> step :: TiState -> TiState
> step state =dispatch (hlookup heap a)
>   where a : stack = tiStack state
>         heap = tiHeap state
>         dispatch (NNum n) = error "number applied as a function"
>         dispatch (NAp a1 a2) = apStep state a1 a2
>         dispatch (NSupercomb name args body) = scStep state name args body

`apStep` is a direct translation of our first transition rule, which unwinds the spine.

> apStep :: TiState -> Addr -> Addr -> TiState
> apStep state a1 a2 = state { tiStack = a1 : stack }
>   where stack = tiStack state

`scStep` is a translation of our second transition rule, which reduces a supercombinator
application.

> scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
> scStep state scName argNames body = state { tiStack = stack', tiHeap = heap' }
>   where stack = tiStack state
>         heap = tiHeap state
>         globals = tiGlobals state
>         (heap', addr) = instantiate body heap (argBindings <> globals)
>         argBindings = Map.fromList $ zip argNames (getArgs heap stack)
>         stack' = addr : drop ((length argNames) + 1) stack

> getArgs :: Heap -> [Addr] -> [Addr]
> getArgs heap (sc : stack) = map getArg stack
>   where getArg addr = case hlookup heap addr of
>                         NAp _ a -> a
>                         _ -> error "Unexpected argument node"

`instantiate` is responsible for converting a `CoreExpr` into a `Node` and allocating it
on the heap. It takes as an argument a collection of argument bindings, mapping argument
names to addresses on the heap.

> instantiate :: CoreExpr -> Heap -> Map Name Addr -> (Heap, Addr)
> instantiate (ENum n) heap _ = halloc heap (NNum n)
> instantiate (EAp e1 e2) heap env = let (heap1, a1) = instantiate e1 heap env
>                                        (heap2, a2) = instantiate e2 heap1 env
>                                     in halloc heap2 (NAp a1 a2)
> instantiate (EVar v) heap env = case Map.lookup v env of
>                                   Just a -> (heap, a)
>                                   Nothing -> error $ "undefined name " <> unpack v

These definitions belong in a utils module, but for now they live here.

> type Addr = Int
> data Heap = Heap Addr (Map Addr Node)

> emptyHeap :: Heap
> emptyHeap = Heap 0 mempty

> halloc :: Heap -> Node -> (Heap, Addr)
> halloc (Heap nextAddr h) a = (Heap (nextAddr + 1) (Map.insert nextAddr a h), nextAddr)

> hlookup :: Heap -> Addr -> Node
> hlookup (Heap _ h) a = Map.findWithDefault (error "invalid address") a h

Here we define functions to pretty-print the machine state.

> printStates :: [TiState] -> Doc a
> printStates = vsep . map pretty

> instance Pretty Node where
>   pretty (NNum n) = "NNum" <+> pretty n
>   pretty (NAp a1 a2) = "NAp" <+> pretty a1 <+> pretty a2
>   pretty (NSupercomb name _ _) = "NSupercomb" <+> pretty name

> instance Pretty TiState where
>   pretty TiState { tiStack = stack, tiHeap = heap } = pStack heap stack

> pStack :: Heap -> [Addr] -> Doc a
> pStack heap stack = "Stack [" <> align (vsep (map pStackItem stack)) <> "]"
>   where pStackItem addr = pretty addr <> ":" <+> pStackNode heap (hlookup heap addr)
>         pStackNode heap (NAp fAddr argAddr)
>           = let node = (hlookup heap argAddr)
>              in "NAp" <+> pretty fAddr <+> pretty argAddr <+> parens (pretty node)
>         pStackNode heap node = pretty node

TODO: examples

[intro]: 2019-03-02-implementing-a-functional-language.html

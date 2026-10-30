---
title: Things you can do with pointers
publish: false
---

# Some things you can do with pointers

Pointers are big! You can fit a lot in 8 bytes. Here are some cool things people have done with them.

## Store numbers
It's quite useful for a language to support arbitrary-precision numbers, so you can do maths without losing precision. Ruby, Python, JS, .NET and Haskell all have support for this, usually via a "Bignum" type which represents an arbitrarily-large, arbitrarily-precise number. This is often implemented via an array of digits.

Ordinary integer and floating point types are a fixed size and fit in a single CPU register. Bignums can be much bigger, and typically do not. But even in code that needs Bignums, there are often scenarios where the actual numbers are small, and paying the performance cost of a Bignum for these is not worth it. It's also tedious and error-prone (how?) to convert between ordinary numbers and Bignums. So some languages handle the conversion automatically and offer a single numeric type which will only be a Bignum if it needs to be.

But then we have a problem: how can we tell what kind of number we are working with? Let's say we have this function

```
def add(x, y):
  x + y
```

In order for this function to run, it needs to know how to perform `+` on `x` and `y`. If both are normal integers, it can use a single CPU instruction (e.g. `ADD x1, x1, x2` on arm64). If either is a Bignum, it needs to call a bignum-specific addition function. In dynamic languages, usually we approach this by looking up the specific function to use in a hash table that each value points to. So for example if `x` were a Bignum it would be a pointer to some memory location, and at that location we would have a structure that looks something like this:

```
{ methods: HashTable<Symbol, Function>, value: Array<...> }
```

To find the function for `+` we search the hash table for an entry with key `+`.

But if `x` is an ordinary 64-bit integer... it's not a pointer, so there's no hash table of methods we can go to. Even worse, we can't even tell that it's an integer, because pointers also just look like 64-bit integers! In a statically-typed language this isn't a problem because the compiler knows the types of everything and can distinguish between these two cases when it compiles the code. But in a dynamically-typed language at runtime, we just can't tell.

The solution to this is to _tag_ the pointer. Basically we overwrite some bits in the pointer to store our own data. In this case we only need to store a single bit. We set it to 0 if the pointer is an actual pointer to an object, and 1 if it's actually a raw integer. Then the code checks this bit and knows whether to follow the pointer to look up the method, or use a basic addition instruction.

But how is this OK? If we start flipping bits in pointers, aren't they going to point to the wrong place? Well in general yes, but as we know, the 64-bit address space is huge (see [Virtual Memory](/posts/2026-08-27-virtual-memory.html)), and the highest bits are often not used. In Linux, bits 48-63 of a pointer are always set to 0. In addition, most operating systems will always allocate memory with an alignment of 16 bytes, meaning that the start address of any allocated memory will always have the low 4 bits set to 0. So if we ensure that our objects are always stored on the heap at aligned addresses, setting the lowest bit of a pointer to 0 has no effect, because it is already 0. Some pointer tagging schemes use the high bits and some use the low bits, depending on their particular constraints (e.g. portability across architectures).

Setting the low bit to 1 for a normal integer means that we have one fewer bit to store the actual number in, so our integers are limited to a max value of $2^{63} - 1$. But this is often sufficient, and if we need to store a bigger number, we just convert it to a Bignum.

This general idea, of borrowing unused bits of a pointer to store metadata, is very common anywhere that performance is important. The other examples in this post are largely just applications of the same concept.

## Store strings

Like Bignums, strings can be arbitrarily large, but often aren't. We can use the same trick here. 63 bits gives room for 7 bytes and 7 bits left over, which is enough for 7 ASCII characters. But we can do better: most strings use a smaller subset of the ASCII alphabet. If we restrict ourselves to just upper and lowercase letters and digits, that's 61 variants. That will fit in 6 bits ($2^6 = 64$) with 3 slots left for other common characters like space or dot. With 6 bits per character we can store 9 characters, with 12 bits left for the length (3 bits) and tag (1 bit). We can't store 10 characters because that would leave only 4 bits for the tag and length, but a length of 10 needs 4 bits to represent ($2^3 = 8$, $2^4 = 16$).

This is roughly the approach that Objective-C takes, although it is even more clever: it uses three different encodings depending on the size of the string and what characters it uses:
- for strings up to length 7 , it stores them in the pointer as plain ASCII
- for strings of length 8 or 9, it uses a custom alphabet of common characters to make a 6-bit encoding. If the string contains only characters in this alphabet, it can be stored inside its pointer.
- for strings of length 10 or 11, it uses half the alphabet of the 8-9 case to make a 5-bit encoding.

This is the alphabet[^1]:
```
eilotrm.apdnsIc ufkMShjTRxgC4013bDNvwyUL2O856P-B79AFKEWV_zGJ/HYX
```

There's a [fantastic blog post](https://mikeash.com/pyblog/friday-qa-2015-07-31-tagged-pointer-strings.html) by Mike Ash which describes this encoding in more detail. Swift, Objective-C's successor, has an [even more complex](https://www.swift.org/blog/utf8-string/) string representation, I think partly because it aims to provide efficient interop with Objective-C code.

## German Strings

If we allow ourselves more than 64 bits to store strings, we can do more interesting things. Analytics systems have to deal with a lot of strings, and want to keep them in CPU registers as much as possible to increase performance. They also often want to do prefix matching: for example to run the SQL query `SELECT * FROM companies WHERE name = 'Oxide Computer Company'`, we want to quickly rule out any rows starting with a letter that isn't `O`.

Many of these systems converged on a design called German Strings. The CedarDB team have [two](https://cedardb.com/blog/german_strings/) [excellent](https://cedardb.com/blog/strings_deep_dive/) blog posts on this, which you should just go read. I'll just mention the highlights.

German Strings are 16 bytes, so they must be passed in two CPU registers. As with Objective-C, small strings up to 12 characters are stored in the pointer, which is tagged. Strings longer than 12 characters are stored on the heap, but only 62 bits are used to point to this. 32 bits are used to hold the length of the string, and another 32 bits hold the first 4 characters of the string.

```
Short strings (<= 12 chars)

| 32 bits |                       96 bits                        |
 ----------------------------------------------------------------
| length  |                       content                        |
 ----------------------------------------------------------------

Long strings (> 12 chars)

| 32 bits | 32 bits |2 bits|              62 bits                 |
 -----------------------------------------------------------------
| length  | prefix  |class |              pointer                 |
 -----------------------------------------------------------------
```

This layout has a few advantages. Firstly, the prefix allows us to do fast prefix filtering without fetching the full string from the heap. But cleverly, we can do this prefix filtering on _either_ string representation. In both cases, bytes 5-8 hold the first 4 UTF-8 bytes of the string, so code doesn't need to branch on the string type and can just plough on regardless.

Another interesting point is that there's no explicit tag to indicate what representation is being used. Instead, we can tell from the length. If it's <=12, we know it's a short string.

The `class` is used to distinguish between different types of heap-allocated string - immutable, mutable and transient. The blog post describes this in more detail. Transient is an interesting case that relates to the next thing we'll talk about. 

## Pointer Swizzling

We've seen how a pointer can be tagged to tell us if it is a real pointer, or just some data. Pointer swizzling is tagging a pointer to tell us if it is a real pointer, or a reference to something on disk. A particularly interesting application of this technique is in [LeanStore](https://doi.org/10.1109/ICDE.2018.00026) ([PDF](https://15721.courses.cs.cmu.edu/spring2020/papers/23-largethanmemory/leis-icde2018.pdf)), which is a database storage manager.

A storage (or buffer) manager is a component in a database which is responsible for keeping track of where the data is stored on disk, and copying it to/from memory when needed. In traditional databases, data is divided into pages which are stored on disk and then copied into memory to be read. When the page is written to in memory, the buffer manager is then responsible for writing it back to disk. Pages are usually kept in memory as long as possible, and when the database is low on memory it will evict pages to disk. The rest of the database only refers to pages via logical IDs which the manager translates into memory addresses on-demand[^2].

The downside of this approach is that the buffer manager typically keeps track of pages in a big hash table, and every time a page is used, the manager must query the hash table to get its address. When the page is already in memory, this is pure overhead, and this overhead can be quite significant once the rest of the system is optimised.

In LeanStore, pages are referred to directly by a pointer to their memory, so there's no hash table lookup required. If the page data isn't actually in memory yet, the pointer instead holds the logical page ID instead. And you guessed it - the pointer is tagged to indicate which is the case. Pointers to in-memory pages are _swizzled_ and pointers to on-disk pages are _unswizzled_[^3].

This is basically the main idea, but there's another thing that LeanStore does that I think is particularly cool. When memory is scarce, a buffer manager has to decide what pages to evict, and the accounting required for this can be an additional overhead. For example with a Least Recently Used policy, each page must store a counter or timestamp indicating when it was last used, and this must be updated on every read.

LeanStore doesn't store any extra metadata on pages for this. Instead, it periodically picks a random in-memory page and unswizzles its pointer, but doesn't actually evict the page from memory. The page is placed into a queue of "cooling" pages, and when it reaches the front of the queue its memory will be evicted. While waiting in the queue, if the database reads that page then it will re-swizzle the pointer and remove it from the queue.

The queue is scaled to hold around 10% of all pages at any time. If a frequently accessed page is added to the queue, it will quickly be removed by a subsequent access. If an infrequently accessed page is added, it is likely to remain until it reaches the front of the queue, and will be evicted. As the paper says:

> To summarize, by speculatively unswizzling random pages, we identify infrequently-accessed pages without having to track each access.

This feels like a tactic that we could apply to other problems, too. If you want to classify a pool of things without keeping metadata on each one, randomly move some percentage of them to one side and let their behaviour self-correct it. The analogies I can think of are garbage collection and shutting down unused resources, but I'm sure there are others.

[^1]: If you've studied any cryptography, you might recognise the first few characters as the [most common characters](https://en.wikipedia.org/wiki/Etaoin_shrdlu) in the English language.

[^2]: In some ways this general idea is reminiscent of how an OS manages its own page cache of data read from disk, and how it manages memory that has been swapped to disk when the system is low on physical RAM.

[^3]: No idea.

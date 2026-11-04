---
title: Things you can do with pointers
publish: false
---

# Some things you can do with pointers

Pointers are big! You can fit a lot in 8 bytes. Here are some cool things people have done with them.

## Store numbers
Numbers can be stored in many different ways, because there are many kinds of numbers. Positive integers under $2^{64}$ can be stored in binary form in a single 64-bit register. The same applies to negative integers between $-2^{63}$ and $2^{63} - 1$. Non-integers like $1.23$ can be stored as floating point numbers, though with limited precision. If we want higher precision floats or bigger integers, we can use larger blocks of memory: double-precision floats or 128-bit ints.

But all of these options have limits: there's no way to store an arbitrarily-large or precise number. Obviously in some domains it's necessary to do calculations on big numbers and get the right answer, and for that we can use "bignums". I think of these as basically arrays of digits that can be as long as they need to be. Operations over bignums are slower than over simple numeric types but we can represent more numbers faithfully[^5].

There are also scenarios where we're dealing with a large quantity of small numbers and even allocating 64 bits for each number is too wasteful. Consider a 1024x1024 matrix of positive integers under 16. At 64 bits per integer this is 8MB, but at 4 bits per integer it's only 512KB. The V8 Javascript engine applies this sort of optimisation by defining a type `Smi` (small integer) which holds any integer that fits in 32 bits. An `Smi` looks like this:

```
            |----- 32 bits -----|----- 32 bits -----|
Pointer:    |________________address_______________1|
   Smi:     |____int32_value____|00................0|

```

It's a 64-bit value that can be passed in a register, but only the top 32 bits hold the actual number. The bottom bit is a _tag_. The lowest bit is always set to 0, and this means that this number is an `Smi`[^6]. When v8 executes an operation on a number, it checks whether it is an `Smi` by testing the low bit, which is very cheap to do, and then uses `Smi`-specific code for that operation. If an operation produces a result that doesn't fit in an `Smi` then v8 falls back to allocating a number object on the heap.

There are two main benefits of this optimisation:

- Small numbers use less memory. `Smi` is not allocated on the heap, it only exists in its own pointer. So storing an array of `Smi` numbers costs just 8 bytes per number. If these were normal heap numbers instead, the array would still hold 8 bytes per number but each number would point to another 16-byte object elsewhere in the heap that we would also need to allocate. So total memory per number would be 24 bytes.
- Small numbers are faster to work with. Adding two `Smi` numbers is simple: a single 64-bit `ADD` instruction on the two values gives a 64-bit value with the result in the top 32 bits. If the result is larger than 32 bits, the CPU will set an overflow flag and the code can convert the numbers to normal heap objects and try again.

But how is this OK? If we start flipping bits in pointers, aren't they going to point to the wrong place? Well in general yes, but as we know, the 64-bit address space is huge (see [Virtual Memory](/posts/2026-08-27-virtual-memory.html)), and the highest bits (e.g. bits 48-63 in Linux) are often not used. The OS allocates memory in pages no smaller than 4KB, though memory allocators will typically further divide this up. `malloc` from GNU libc is said to always[^4] return pointers with a minimum alignment of 16 bytes, meaning that the low 4 bits of the pointer are set to 0. So if we ensure that our objects are always stored on the heap at aligned addresses, setting the lowest bit of a pointer to 0 has no effect, because it is already 0. Some pointer tagging schemes use the high bits and some use the low bits, depending on their particular constraints (e.g. portability across architectures).

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

We've seen how a pointer can be tagged to tell us if it is a real pointer, or just some data. Pointer swizzling is tagging a pointer to tell us if it is a real pointer, or a reference to something on disk. A particularly interesting application of this technique is in [LeanStore](https://doi.org/10.1109/ICDE.2018.00026) ([PDF](https://15721.courses.cs.cmu.edu/spring2020/papers/23-largethanmemory/leis-icde2018.pdf)), a database buffer manager.

A buffer manager is a component in a database which is responsible for keeping track of where the data is stored on disk, and copying it to/from memory when needed. Data in traditional databases is divided into pages which are stored on disk and then copied into memory to be read. When the page is written to in memory, the buffer manager is then responsible for writing it back to disk. Pages are kept in memory as long as possible, and when the database is low on memory it will evict pages to disk. The rest of the database only refers to pages via logical IDs which the manager translates into memory addresses on-demand[^2].

The downside of this approach is that the buffer manager typically keeps track of pages in a big hash table, and every time a page is needed its address must be fetched from the hash table. When the page is already in memory this is pure overhead, and this overhead can be quite significant once the rest of the system is optimised.

In LeanStore, pages are referred to directly by a pointer to their memory, so there's no hash table lookup required. If the page data isn't actually in memory yet, the pointer instead holds the logical page ID instead. And the pointer is tagged to indicate which is the case. Pointers to in-memory pages are _swizzled_ and pointers to on-disk pages are _unswizzled_[^3]. With just this optimisation, LeanStore gets a significant speed boost: 1.3-1.6x higher transaction throughput than without it.

This is basically the main idea, but there's another thing that LeanStore does that I think is particularly cool. When memory is scarce, a buffer manager has to decide what pages to evict, and the accounting required for this can be an additional overhead. For example with a Least Recently Used policy, each page must store a counter or timestamp indicating when it was last used, and this must be updated on every read.

LeanStore doesn't store any extra metadata on pages for this. Instead, it periodically picks a random in-memory page and unswizzles its pointer, but doesn't actually evict the page from memory. The page is placed into a queue of "cooling" pages, and when it reaches the front of the queue its memory will be evicted. While waiting in the queue, if the database reads that page then it will re-swizzle the pointer and remove it from the queue.

The queue is scaled to hold around 10% of all pages at any time. If a frequently accessed page is added to the queue, it will quickly be removed by a subsequent access. If an infrequently accessed page is added, it is likely to remain until it reaches the front of the queue, and will be evicted. As the paper says:

> To summarize, by speculatively unswizzling random pages, we identify infrequently-accessed pages without having to track each access.

This feels like a tactic that we could apply to other problems, too. If you want to classify a pool of things without keeping metadata on each one, randomly move some percentage of them to one side and let their behaviour self-correct it. The analogies I can think of are garbage collection and shutting down unused resources, but I'm sure there are others.

[^1]: If you've studied any cryptography, you might recognise the first few characters as the [most common characters](https://en.wikipedia.org/wiki/Etaoin_shrdlu) in the English language.

[^2]: In some ways this general idea is reminiscent of how an OS manages its own page cache of data read from disk, and how it manages memory that has been swapped to disk when the system is low on physical RAM.

[^3]: No idea.

[^4]: Documented [here](https://sourceware.org/glibc/wiki/MallocInternals) with the caveat "for entertainment purposes only, and are not guaranteed to be correct"

[^5]: Of course there's a limit even here. We only have so much memory available, and can't truly store the exact value of $\pi$. But we can usually store enough of it that the answer to our calculation is correct up to the precision we need.

[^6]: This is simplified slightly. The layout is different on 32-bit systems and also different if _pointer compression_ is enabled (a v8 feature that sounds very interesting but I haven't researched). The second-lowest bit is also a tag which is used to distinguish between different types of pointers, I think for the garbage collector, but that isn't relevant for our discussion.

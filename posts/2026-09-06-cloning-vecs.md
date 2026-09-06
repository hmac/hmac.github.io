---
title: Cloning Vecs
publish: true
---

# Cloning Vecs

The other day I hit a confusing case with `repeat_n` that took me a while to figure out.

I needed to allocate 64 `Vec`s which would each hold a lot of data, so I used `Vec::with_capacity` to create them with a large capacity up front and avoid the cost of growing. Instead of writing that out 64 times I used `repeat_n`:

```rust
std::iter::repeat_n(Vec::with_capacity(4096), 64).collect()
```

But this [doesn’t actually](https://play.rust-lang.org/?version=stable&mode=debug&edition=2024&gist=65f3a3c776e6cdfe6d24cc0643e39ff8) give 64 vectors each with a capacity of 4096, as you might expect. 

```rust
fn main() {
    let x: Vec<Vec<usize>> = std::iter::repeat_n(Vec::with_capacity(4096), 64)
	    .collect();

    dbg!(x.iter().map(|a| a.capacity()).collect::<Vec<_>>());
}
```

```
[src/main.rs:5:5] x.iter().map(|a| a.capacity()).collect::<Vec<_>>() = [
    0,
    0,
    // ...
    0,
    0,
    4096,
]
```

Instead, you get one vector with a capacity of 4096 preceded by 63 vectors with a capacity of 0. This happens because `repeat_n` [works](https://doc.rust-lang.org/src/core/iter/sources/repeat_n.rs.html#59) by cloning its argument n-1 times, then yielding the original argument as the last element.

```rust
impl<A: Clone> Iterator for RepeatN<A> {
    type Item = A;

    fn next(&mut self) -> Option<A> {
        let inner = self.inner.as_mut()?;
        let count = inner.count.get();

        if let Some(decremented) = NonZero::<usize>::new(count - 1) {
            // Order of these is important for optimization
            let tmp = inner.element.clone();
            inner.count = decremented;
            return Some(tmp);
        }

        return self.take_element();
    }
}
```

And when you clone a `Vec`, its capacity gets reset to equal its length. This happens via a slightly convoluted chain of function calls: `Vec<T>::clone` delegates to [`<[T]>::to_vec_in`](https://doc.rust-lang.org/src/alloc/slice.rs.html#396-398), i.e. a slice method, whose implementation defines an inline trait `ConvertVec` and `impl ConvertVec for T`:

```rust
pub fn to_vec_in<A: Allocator>(&self, alloc: A) -> Vec<T, A>
    where
        T: Clone,
    {
        return T::to_vec(self, alloc);

        trait ConvertVec {
            fn to_vec<A: Allocator>(s: &[Self], alloc: A) -> Vec<Self, A>
            where
                Self: Sized;
        }

        impl<T: Clone> ConvertVec for T {
            #[inline]
            default fn to_vec<A: Allocator>(s: &[Self], alloc: A) -> Vec<Self, A> {
                struct DropGuard<'a, T, A: Allocator> {
                    vec: &'a mut Vec<T, A>,
                    num_init: usize,
                }
                impl<'a, T, A: Allocator> Drop for DropGuard<'a, T, A> {
                    #[inline]
                    fn drop(&mut self) {
                        // SAFETY:
                        // items were marked initialized in the loop below
                        unsafe {
                            self.vec.set_len(self.num_init);
                        }
                    }
                }
                let mut vec = Vec::with_capacity_in(s.len(), alloc);
                let mut guard = DropGuard { vec: &mut vec, num_init: 0 };
                let slots = guard.vec.spare_capacity_mut();
                // .take(slots.len()) is necessary for LLVM to remove bounds checks
                // and has better codegen than zip.
                for (i, b) in s.iter().enumerate().take(slots.len()) {
                    guard.num_init = i;
                    slots[i].write(b.clone());
                }
                core::mem::forget(guard);
                // SAFETY:
                // the vec was allocated and initialized above to at least this length.
                unsafe {
                    vec.set_len(s.len());
                }
                vec
            }
        }
    // ...
}
```

We can see that this code allocates a `Vec` with a capacity of `s.len()`, which explains the behaviour I was seeing. The rest of the function basically iterates over the slice, clones each element and stores it in the `Vec`. But it does it by directly writing to the uninitialised slots in the `Vec`, instead of the normal `Vec::push`. When all elements are written it then initialises the whole thing via `Vec::set_len`. I can only assume this is a performance optimisation that skips some of the overhead with `Vec::push`. If we look at the code for `Vec::push` (well, for [`push_mut`](https://doc.rust-lang.org/src/alloc/vec/mod.rs.html#1033) since `push` just delegates to it):

```rust
pub fn push_mut(&mut self, value: T) -> &mut T {
	// Inform codegen that the length does not change across grow_one().
	let len = self.len;
	// This will panic or abort if we would allocate > isize::MAX bytes
	// or if the length increment would overflow for zero-sized types.
	if len == self.buf.capacity() {
		self.buf.grow_one();
	}
	unsafe {
		let end = self.as_mut_ptr().add(len);
		ptr::write(end, value);
		self.len = len + 1;
		// SAFETY: We just wrote a value to the pointer that will live the lifetime of the reference.
		&mut *end
	}
}
```

There are two bits of this that are technically unnecessary when we're cloning a `Vec`. We check to see if we've hit capacity and need to grow, which won't happen because we allocated the Vec to `have` enough capacity. We also increment `self.len`, which we will do repeatedly for each element we push.

So instead, `to_vec_in` skips the length check and writes to `self.len` once, at the end. The `DropGuard` seems intended to ensure that `Vec::set_len` is called, even if we only copy some of the slice elements. I think this might be to protect against the case where cloning the element panics. In that scenario we would attempt to drop `vec` as we unwind the stack, but the `Drop` code for `Vec` assumes that its length is set correctly, so the `DropGuard` makes sure of that.

Why is this implemented via an inline trait definition? I do not know.

So anyway, if you want to initialise a bunch of `Vec`s with a specific capacity, don't use `repeat_n`. I used this instead:

```rust
(0..64)
  .map(|_| Vec::with_capacity(4096))
  .collect::<Vec<_>>()
```

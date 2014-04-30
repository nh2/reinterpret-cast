float-cast
==========

Memory reinterpretation casts for Float/Double and Word32/Word64 in Haskell.

Takes the bit representation of a number and uses it for a different numeric type.
This is `reinterpret_cast` from C++ and `float f = 1.23; int i = * (int *) &f;` from C.

We offer you fast casts for:

```haskell
wordToFloat  :: Word32 -> Float
floatToWord  :: Float  -> Word32
wordToDouble :: Word64 -> Double
doubleToWord :: Double -> Word64
```

If you need something like `Int32` or similar, `fromIntegral` will do the job
within the integral types (so to/from `Word*` are the only conversions needed).

On [Hackage](http://hackage.haskell.org/package/float-cast): `cabal install float-cast`


Fast casting
------------

The way implemented in this package is the fastest possible way known as of now. In particular, it is faster than what [`data-binary-ieee754`](https://hackage.haskell.org/package/data-binary-ieee754) does at the moment.

For a discussion, see [this StackOverflow](http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa) question.

Benchmark results for `bench/Bench.hs` are [available here](https://rawgit.com/nh2/float-cast/master/bench-results/results.html).

In theory, these should all be no-ops, but you [must not use `unsafeCoerce`](https://ghc.haskell.org/trac/ghc/ticket/4092) or even `unsafeCoerce#` for this.

The real solution will be [this GHC feature request](https://ghc.haskell.org/trac/ghc/ticket/4092) - please support it.


TODO
----

* Compare to the performance of an `unsafe` pure FFI call that does `float f = 1.23; int i = * (int *) &f;` inside.

Contributions welcome!

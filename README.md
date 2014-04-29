float-cast
==========

Converts between Float/Double and Word32/Word64 in Haskell.

This provides you with 4 fast casts:

```haskell
wordToFloat  :: Word32 -> Float
floatToWord  :: Float  -> Word32
wordToDouble :: Word64 -> Double
doubleToWord :: Double -> Word64
```

On [Hackage](http://hackage.haskell.org/package/float-cast): `cabal install float-cast`


Fast casting
------------

The way implemented in this package is the fastest possible way known as of now.

For a discussion, see [this StackOverflow](http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa) question.

Benchmark results for `bench/Bench.hs` are [available here](https://rawgit.com/nh2/float-cast/master/bench-results/results.html).

In theory, these should all be no-ops, but you [must not use `unsafeCoerce`](https://ghc.haskell.org/trac/ghc/ticket/4092) or even `unsafeCoerce#` for this.

The real solution will be [this GHC feature request](https://ghc.haskell.org/trac/ghc/ticket/4092) - please support it.

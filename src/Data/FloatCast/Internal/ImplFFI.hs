-- | This is the 'FFI' approach.
--
-- Implements casting via the FFI, using `alloca` like in
-- http://hackage.haskell.org/package/data-binary-ieee754.
module Data.FloatCast.Internal.ImplFFI
  ( wordToFloat
  , floatToWord
  , wordToDouble
  , doubleToWord
  ) where

import qualified Foreign as F
import           System.IO.Unsafe (unsafePerformIO)


{-# INLINABLE floatToWord #-}
floatToWord :: Float -> F.Word32
floatToWord = fromFloat


{-# INLINABLE wordToFloat #-}
wordToFloat :: F.Word32 -> Float
wordToFloat = toFloat


{-# INLINABLE doubleToWord #-}
doubleToWord :: Double -> F.Word64
doubleToWord = fromFloat


{-# INLINABLE wordToDouble #-}
wordToDouble :: F.Word64 -> Double
wordToDouble = toFloat


{-# INLINE toFloat #-}
toFloat :: (F.Storable word, F.Storable float) => word -> float
toFloat word = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) word
  F.peek buf


{-# INLINE fromFloat #-}
fromFloat :: (F.Storable word, F.Storable float) => float -> word
fromFloat float = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) float
  F.peek buf

-- | This is the 'FFI' approach.
--
-- Implements casting via the FFI, using `alloca` like in
-- <http://hackage.haskell.org/package/data-binary-ieee754>.
module Data.ReinterpretCast.Internal.ImplFFI
  ( wordToFloat
  , floatToWord
  , wordToDouble
  , doubleToWord
  ) where

import qualified Foreign as F
import           System.IO.Unsafe (unsafePerformIO)


floatToWord :: Float -> F.Word32
floatToWord = fromFloat

{-# INLINABLE floatToWord #-}


wordToFloat :: F.Word32 -> Float
wordToFloat = toFloat

{-# INLINABLE wordToFloat #-}


doubleToWord :: Double -> F.Word64
doubleToWord = fromFloat

{-# INLINABLE doubleToWord #-}


wordToDouble :: F.Word64 -> Double
wordToDouble = toFloat

{-# INLINABLE wordToDouble #-}


toFloat :: (F.Storable word, F.Storable float) => word -> float
toFloat word = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) word
  F.peek buf

{-# INLINE toFloat #-}


fromFloat :: (F.Storable word, F.Storable float) => float -> word
fromFloat float = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) float
  F.peek buf

{-# INLINE fromFloat #-}

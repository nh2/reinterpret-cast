-- | Converts between Float/Double and Word32/Word64.
--
-- Currently we use the 'array' method from http://stackoverflow.com/a/7002812/263061.
module Data.FloatCast
  ( Impl.wordToFloat
  , Impl.floatToWord
  , Impl.wordToDouble
  , Impl.doubleToWord
  ) where

import qualified Data.FloatCast.Internal.ImplArray as Impl

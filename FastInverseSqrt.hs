{-# LANGUAGE MagicHash, UnboxedTuples #-}
module FastInverseSqrt where
import Data.Bits

import Foreign.Marshal.Alloc
import Foreign.Marshal.Unsafe
import Foreign.Ptr
import Foreign.Storable

import GHC.Prim
import GHC.Types

import GHC.ST

import Criterion.Main
import System.Random
import Test.QuickCheck

--- Implementation(s)

-- These work, but cause fastInverseSqrt to be half
-- the speed of slowInverseSqrt, even on -O2
castStorable x = unsafeLocalState . alloca $ \p -> poke p x >> peek (castPtr p)

intToDouble = castStorable :: Int -> Double
doubleToInt = castStorable :: Double -> Int

{-
intToDouble (I# x) = runST $ ST action where
    action s = case newByteArray# 8# s of
        (# s, arr #) -> case writeIntArray# arr 0# x s
            of s -> case readDoubleArray# arr 0# s
                of (# s, y #) -> (# s, D# y #)

doubleToInt (D# x) = runST $ ST action where
    action s = case newByteArray# 8# s of
        (# s, arr #) -> case writeDoubleArray# arr 0# x s
            of s -> case readIntArray# arr 0# s
                of (# s, y #) -> (# s, I# y #)
-}

{-
-- These only work in unoptimized/interpreted, and cause
-- GHC to output ill-formed assembly if -O{1,2} are on
intToDouble (I# x) = D# (unsafeCoerce# x)
doubleToInt (D# x) = I# (unsafeCoerce# x)
-}

-- http://en.wikipedia.org/wiki/Fast_inverse_square_root references
-- http://shelfflag.com/rsqrt.pdf, which contains the source of the magic
-- number for doubles, and claims a maximum relative error of 0.0017511837
fastInverseSqrt x = y' where
    magic = 0x5fe6eb50c7b537a9
    y = intToDouble (magic - shiftR (doubleToInt x) 1)
    y' = y * (1.5 - (x/2 * y * y))

{-
-- This version only works in interpreted, and causes different
--  ill-formed assembly based on the optimization level
fastInverseSqrt (D# x) = (D# y') where
    magic = 0x5fe6eb50c7b537a9#
    y' = case (unsafeCoerce# (magic -# uncheckedIShiftRA# (unsafeCoerce# x) 1#)) of
        y -> y *## (1.5## -## (x/##(2.0##) *## y *## y))
-}

--- Testing / Benchmarking

slowInverseSqrt x = 1 / sqrt x

absRelErr x y = abs ((x-y)/x)

positive = arbitrary `suchThat` (0 <=)

prop_smallError = forAll positive $ \x ->
    absRelErr (slowInverseSqrt x) (fastInverseSqrt x) < 0.0017511837

benchmark = defaultMain [bgroup "fastInverseSqrt" $ concatMap (\x -> [
        bench ("fast_"++show x) $ nf fastInverseSqrt x,
        bench ("slow_"++show x) $ nf slowInverseSqrt x
    ]) (take 5 $ randomRs (1e-3, 1e3) (mkStdGen 0))]

{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.List
import Test.QuickCheck

partitionByST cmp vec i j = do
    let pivotIdx = (i + j) `div` 2
    pivotVal <- VM.read vec pivotIdx
    VM.swap vec pivotIdx j
    storeIdx <- newSTRef i
    forM_ [i..j-1] $ \i -> do
        val <- VM.read vec i
        when (cmp val pivotVal == LT) $ do
            readSTRef storeIdx >>= VM.swap vec i
            modifySTRef storeIdx (+1)
    readSTRef storeIdx >>= VM.swap vec j
    readSTRef storeIdx >>= return

quicksortByST cmp vec = aux 0 (VM.length vec - 1) >> return () where
    aux i j = when (i < j) $ do
        mid <- partitionByST cmp vec i j
        aux i (mid-1)
        aux (mid+1) j

quicksortBy cmp vec = V.modify (quicksortByST cmp) vec

quicksort = quicksortBy compare

runTests = verboseCheck matchesListSort where
    matchesListSort :: [Int] -> Bool
    matchesListSort x = (sort x) == (V.toList . quicksort $ V.fromList x)

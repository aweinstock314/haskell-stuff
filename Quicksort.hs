{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.List
import Test.QuickCheck

partitionByST cmp vec lo hi = do
    let pivotIdx = (lo + hi) `div` 2
    pivotVal <- VM.read vec pivotIdx
    VM.swap vec pivotIdx hi
    storeIdx <- newSTRef lo
    forM_ [lo..hi-1] $ \i -> do
        val <- VM.read vec i
        when (cmp val pivotVal == LT) $ do
            readSTRef storeIdx >>= VM.swap vec i
            modifySTRef storeIdx (+1)
    readSTRef storeIdx >>= VM.swap vec hi
    readSTRef storeIdx >>= return

quicksortByST cmp vec = aux 0 (VM.length vec - 1) where
    aux lo hi = when (lo < hi) $ do
        mid <- partitionByST cmp vec lo hi
        aux lo (mid-1)
        aux (mid+1) hi

quicksortBy cmp vec = V.modify (quicksortByST cmp) vec

quicksort = quicksortBy compare

runTests = verboseCheck matchesListSort where
    matchesListSort :: [Int] -> Bool
    matchesListSort x = (sort x) == (V.toList . quicksort $ V.fromList x)

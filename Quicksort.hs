{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.List
import Test.QuickCheck

{-
-- http://en.wikipedia.org/wiki/Quicksort
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
-}

-- http://en.wikipedia.org/wiki/Dutch_national_flag_problem
partitionByST cmp vec lo hi = VM.read vec pivotIdx >>= aux lo lo hi where
    pivotIdx = lo + ((hi-lo) `div` 2)
    aux i j n mid | j <= n = do
        a_j <- VM.read vec j
        case cmp a_j mid of
            LT -> VM.swap vec i j >> aux (i+1) (j+1) n mid
            GT -> VM.swap vec j n >> aux i j (n-1) mid
            EQ -> aux i (j+1) n mid
    aux i j n mid = return (i, j)

quicksortByST cmp vec = aux 0 (VM.length vec - 1) where
    aux lo hi = when (lo < hi) $ do
        (leftMid, rightMid) <- partitionByST cmp vec lo hi
        aux lo leftMid
        aux rightMid hi

quicksortBy cmp vec = V.modify (quicksortByST cmp) vec

quicksort = quicksortBy compare

runTests = verboseCheck matchesListSort where
    matchesListSort :: [Int] -> Bool
    matchesListSort x = (sort x) == (V.toList . quicksort $ V.fromList x)

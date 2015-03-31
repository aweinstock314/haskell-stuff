{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Criterion.Main
import Data.List (sort)
import System.Random
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

naiveQuicksort [] = []
naiveQuicksort lst = naiveQuicksort less ++ eq ++ naiveQuicksort more where
    pivot = lst !! (length lst `div` 2)
    [less, eq, more] = map (\op -> filter (`op` pivot) lst) [(<), (==), (>)]

-- http://en.wikipedia.org/wiki/Dutch_national_flag_problem
partitionByST cmp vec pivot lo hi = aux lo lo hi where
    aux i j n | j <= n = do
        a_j <- VM.read vec j
        case cmp a_j pivot of
            LT -> VM.swap vec i j >> aux (i+1) (j+1) n
            GT -> VM.swap vec j n >> aux i j (n-1)
            EQ -> aux i (j+1) n
    aux i j n = return (i, j)

quicksortByST cmp vec = aux 0 (VM.length vec - 1) where
    aux lo hi = when (lo < hi) $ do
        let pivotIdx = lo + ((hi-lo) `div` 2)
        pivot <- VM.read vec pivotIdx
        (leftMid, rightMid) <- partitionByST cmp vec pivot lo hi
        aux lo leftMid
        aux rightMid hi

quicksortBy cmp vec = V.modify (quicksortByST cmp) vec

quicksort = quicksortBy compare

quicksortList = V.toList . quicksort . V.fromList

runTests = verboseCheck naiveSort_matches_STSort where
    naiveSort_matches_STSort :: [Int] -> Bool
    naiveSort_matches_STSort x = naiveQuicksort x == quicksortList x

runBenchmarks n = do
    let sampleDataList = take n . randoms $ mkStdGen 0 :: [Int]
    let sampleDataVec = V.fromList sampleDataList
    defaultMain . return $ bgroup "quicksort" [
        bench "standardSort" $ nf sort sampleDataList,
        bench "naiveSort" $ nf naiveQuicksort sampleDataList,
        bench "STSort" $ nf quicksort sampleDataVec,
        bench "STListSort" $ nf quicksortList sampleDataList
        ]

main = runBenchmarks 100000

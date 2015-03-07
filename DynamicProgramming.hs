import Data.Ix
import Data.List
import Data.Ord
import Data.Vector ((!))
import qualified Data.Vector as V

dpFix n f = result where
    lookup i = f (\j -> if inRange (0, n) j then result ! j else f lookup j) i
    result = V.generate n lookup

fibonacci n = (dpFix n fib') ! (n-1) where
    fib' _ 0 = 1
    fib' _ 1 = 1
    fib' rec n = rec (n-1) + rec (n-2)

longestIncreasingSubsequence sequence = result where
    f _ (-1) = ([], 0)
    f rec i = let si = sequence ! i in maximumBy (comparing snd) $ concat [
            map (\j -> case rec j of {
                (x:xs, k) | x < si -> (si:x:xs,k+1);
                (_, _) -> ([si], 1)
            }) [-1..i-1]
        ]
    n = V.length sequence
    result = reverse . fst $ V.maximumBy (comparing snd) (dpFix n f)


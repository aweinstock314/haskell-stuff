{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Queue.Allison
import Control.Monad.ST
import Control.Monad.Trans
import Criterion.Main
import Data.Conduit
import Data.Word
import qualified Data.Conduit.List as CL
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

incrementPermutation (lo, hi) state = aux (VM.length state - 1) where
    aux i = when (i >= 0) $ do
        x <- VM.read state i
        if x == hi
            then VM.write state i lo >> aux (i-1)
            else VM.write state i (succ x)

-- Conduit version
permutationsWithReplacement (lo, hi) n = loop (V.replicate n lo) where
    loop v = let
        v' = V.modify (incrementPermutation (lo, hi)) v
        done = V.all (== hi) v
        in if done then yield v else yield v >> loop v'

prop_pWR_equals_replicateM n = lhs == rhs where
    lhs = runST $ runConduit $ permutationsWithReplacement ('a', 'c') n =$= CL.consume
    rhs = map V.fromList $ replicateM n ['a'..'c']

-- Allison queue version
filterPermutationsWithReplacementN (lo, hi) p n = loop (V.replicate n lo) where
    loop v = let
        q = if p v then enQ v else return ()
        v' = V.modify (incrementPermutation (lo, hi)) v
        done = V.all (== hi) v
        in if done then q else q >> loop v'

filterPermutationsWithReplacementTo (lo, hi) p ns = runQueue $
    forM_ ns $ filterPermutationsWithReplacementN (lo, hi) p

prop_fPWR_equals_replicateM n = lhs == rhs where
    lhs = filterPermutationsWithReplacementTo ('a', 'c') (const True) [n]
    rhs = map V.fromList $ replicateM n ['a'..'c']

-- Mains
--main = forM_ [5..15] $ \i -> mapM_ print . take 5 $ filterPermutationsWithReplacementTo (0::Word8, 5) ((== 1) . V.sum) [i]

--main = forM_ [5..15] $ \i -> (runConduit $ permutationsWithReplacement (0::Word8, 5) i =$= CL.filter ((== 1) . V.sum) =$= CL.take 5) >>= mapM_ print

main = do
    let benchmarks f = map (\i -> bench (show i) $ nf f i) [3,5,7]
    print $ all prop_pWR_equals_replicateM [0..6]
    print $ all prop_fPWR_equals_replicateM [0..6]
    defaultMain [ bgroup "permutationsWithReplacement" [
        bgroup "conduit/ac" $ benchmarks
            (\n -> runST $ runConduit $ permutationsWithReplacement ('a','c') n =$= CL.consume),
        bgroup "allison/ac" $ benchmarks
            (\n -> filterPermutationsWithReplacementTo ('a','c') (const True) [n])
        ]]

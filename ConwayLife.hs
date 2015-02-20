{-# LANGUAGE TemplateHaskell #-}
module ConwayLife (makeRandomBoard, wrapIdx, truncIdx, wrapIdxCPS, truncIdxCPS, dogrid, showBoard, evolveBoard, evolveBoardCPS, showBoardMut, evolveBoardMut) where
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.IORef
import Data.List
import Data.Maybe
import MkCached
import System.Random
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import qualified System.IO.Unsafe as UNS

intOfBool :: Bool -> Int
intOfBool True = 1
intOfBool False = 0

showCell True = putStr "#"
showCell False = putStr "_"

--makeRandomBoard arrayCtor gen (w, h) = arrayCtor ((0, 0), (w-1, h-1)) (map (`mod` 2) (randoms gen))

makeRandomBoard arrayCtor gen (w, h) density =
    arrayCtor ((0, 0), (w-1, h-1))
    (map (< density) (randoms gen :: [Double]))

dogrid :: Monad m => (Int, Int) -> (Int -> Int -> m a) -> m b -> m ()
dogrid (w, h) eachIndex eachLine = loop 0 0 where
    loop x y | y == h = return ()
    loop x y | x == w = eachLine >> loop 0 (y+1)
    loop x y = eachIndex x y >> loop (x+1) y

showBoard dim brd = dogrid dim (\x y -> showCell $ brd!(x, y)) (putStrLn "")

wrapIdxCPS, truncIdxCPS :: (Int, Int) -> (Int, Int) -> ((Int, Int) -> Int) -> Int -> Int
truncIdxCPS (w, h) (x, y) onSuccess onFail =
    if ((0 <= x) && (x < w) && (0 <= y) && (y < h)) then onSuccess (x, y) else onFail
wrapIdxCPS (w, h) (x, y) onSuccess onFail = onSuccess (x `mod` w, y `mod` h)
wrapIdx, truncIdx :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
truncIdx (w, h) (x, y) = guard ((0 <= x) && (x < w) && (0 <= y) && (y < h)) >> return (x, y)
wrapIdx (w, h) (x, y) = Just (x `mod` w, y `mod` h)
adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
adjacents' :: (Int, Int) -> V.Vector (Int, Int)
adjacents' (x, y) = V.filter (/= (x, y)) $ V.generate 9 ((\(dx, dy) -> (x+dx-1, y+dy-1)) . (`divMod` 3))

neighborIdxs handleEdges = V.fromList . mapMaybe handleEdges . adjacents

cacheLookup cacheref f x = do
    cache <- readIORef cacheref
    let calcAndCache = (let y = f x in y `deepseq` ((writeIORef cacheref $ M.insert x y cache) >> return y))
    maybe calcAndCache return $ M.lookup x cache

{-
WARNING: cachedNeighborIdxs is not referentially 
transparent (since it doesn't key based on the function, 
since functions are not members of the Ord typeclass).

This works in practice because handleEdges is chosen at the 
start (as either wrapIdx (w, h) or truncIdx (w, h)) and not 
changed mid-execution.
-}
{-# NOINLINE neighborIdxsCache #-}
neighborIdxsCache = UNS.unsafePerformIO $ newIORef M.empty
cachedNeighborIdxs handleEdges pt = UNS.unsafePerformIO $ cacheLookup neighborIdxsCache (neighborIdxs handleEdges) pt

sumOfNeighbors :: ((Int, Int) -> Maybe (Int, Int)) -> UArray (Int, Int) Bool -> (Int, Int) -> Int
sumOfNeighbors handleEdges brd = V.sum . V.map (intOfBool . (brd!)) . cachedNeighborIdxs handleEdges
sumOfNeighborsCPS :: ((Int, Int) -> ((Int, Int) -> Int) -> Int -> Int) -> UArray (Int, Int) Bool -> (Int, Int) -> Int
sumOfNeighborsCPS handleEdgesCPS brd = V.sum . V.map (\p -> handleEdgesCPS p (intOfBool.(brd!)) 0) . adjacents'
{-
sumOfNeighbors' handleEdges !brd (!x, !y) = aux (-1) (-1) 0 where
    aux 0 0 !acc = aux 1 0 acc
    aux _ 2 !acc = acc
    aux 2 dy !acc = aux (-1) (dy+1) acc
    aux !dx !dy !acc = case handleEdges (x+dx, y+dy) of
        Just (nx, ny) -> aux (dx+1) dy (acc + (intOfBool $ brd!(nx, ny)))
        Nothing -> aux (dx+1) dy acc
-}

amapi :: (IArray a e, IArray a e', Ix i) => (i -> e -> e') -> a i e -> a i e'
amapi f arr = array (bounds arr) (map (\(i, e) -> (i, f i e)) (assocs arr))

{-# INLINE successorCell #-}
-- A cell remains alive if it has 2 or 3 live neighbors, and a cell is born if it has 3 live neighbors
successorCell :: Bool -> Int -> Bool
successorCell True = (`elem` [2, 3])
successorCell False = (== 3)

evolveBoard handleEdges brd = amapi (\(x, y) alive ->
    successorCell alive $ sumOfNeighbors handleEdges brd (x, y)
    ) brd
evolveBoardCPS handleEdgesCPS brd = amapi (\(x, y) alive ->
    successorCell alive $ sumOfNeighborsCPS handleEdgesCPS brd (x, y)
    ) brd

showBoardMut dim brd = dogrid dim (\x y -> readArray brd (x, y) >>= showCell) (putStrLn "")

sumOfNeighborsMut :: ((Int, Int) -> Maybe (Int, Int)) -> IOUArray (Int, Int) Bool -> (Int, Int) -> IO Int
sumOfNeighborsMut handleEdges brd = fmap V.sum . V.mapM (fmap intOfBool . readArray brd) . cachedNeighborIdxs handleEdges

mapArrayI :: (MArray a e m, MArray a e' m, Ix i) => (i -> e -> m e') -> a i e -> m (a i e')
mapArrayI f arr = do
    bounds <- getBounds arr
    arr' <- newArray_ bounds
    flip mapM_ (range bounds) $ \i -> do
        e <- readArray arr i
        e' <- f i e
        writeArray arr' i e'
    return arr'

evolveBoardMut :: ((Int, Int) -> Maybe (Int, Int)) -> IOUArray (Int, Int) Bool -> IO (IOUArray (Int, Int) Bool)
evolveBoardMut handleEdges brd = mapArrayI (\(x, y) alive -> do
    neighborsAlive <- sumOfNeighborsMut handleEdges brd (x, y)
    return $ successorCell alive neighborsAlive
    ) brd

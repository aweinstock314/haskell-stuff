#!/usr/bin/env runghc
{-# LANGUAGE TemplateHaskell #-}
import Control.Exception
import Control.Monad
import Data.Array.Unboxed
import Data.Array.IO
import Data.IORef
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random
import qualified System.IO.Unsafe as UNS
import qualified Data.Map as M
import MkCached
import qualified Data.Vector.Unboxed as V
import Control.DeepSeq

intOfBool :: Bool -> Int
intOfBool True = 1
intOfBool False = 0

showCell True = putStr "#"
showCell False = putStr "_"

--makeRandomBoard arrayCtor gen (w, h) = arrayCtor ((0, 0), (w-1, h-1)) (map (`mod` 2) (randoms gen))

makeRandomBoard arrayCtor gen (w, h) density =
    arrayCtor ((0, 0), (w-1, h-1))
    (map (< density) (randoms gen :: [Double]))

dogrid :: (Int, Int) -> (Int -> Int -> IO ()) -> IO () -> IO ()
dogrid (w, h) eachIndex eachLine = loop 0 0 where
    loop x y | y == h = return ()
    loop x y | x == w = eachLine >> loop 0 (y+1)
    loop x y = eachIndex x y >> loop (x+1) y

showBoard dim brd = dogrid dim (\x y -> showCell $ brd!(x, y)) (putStrLn "")

wrapIdx, truncIdx :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
truncIdx (w, h) (x, y) = guard ((0 <= x) && (x < w) && (0 <= y) && (y < h)) >> return (x, y)
wrapIdx (w, h) (x, y) = Just (x `mod` w, y `mod` h)
adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
neighborIdxs handleEdges pt = V.fromList . mapMaybe handleEdges $ adjacents pt

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

linesBetweenIters = 1
-- Sets the buffering to dump a whole board to stdout at once, to make animation appear smooth(er)
setBoardBuffering w h = hSetBuffering stdout . BlockBuffering . Just $ (w+1)*h + linesBetweenIters
showSpacing = replicateM_ linesBetweenIters $ putStrLn ""

showAutomaton (w, h, loop, edge, seedgen) = do
    setBoardBuffering w h
    gen <- fromMaybe getStdGen $ liftM return seedgen
    let initBoard = makeRandomBoard listArray gen (w, h) 0.3 :: UArray (Int, Int) Bool
    let boards = iterate (evolveBoard (edge (w, h))) initBoard
    loop $ map (\brd -> showBoard (w, h) brd >> showSpacing) boards

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

showAutomatonMut (w, h, loop, edge, seedgen) = do
    setBoardBuffering w h
    gen <- fromMaybe getStdGen $ liftM return seedgen
    boardRef <- (makeRandomBoard newListArray gen (w, h) 0.3 :: IO (IOUArray (Int, Int) Bool)) >>= newIORef
    loop . cycle . return $ do
        board <- readIORef boardRef
        showBoardMut (w, h) board
        showSpacing
        evolveBoardMut (edge (w, h)) board >>= writeIORef boardRef

parse ctor = map (ctor . fst) . reads
data Opt = Width Int | Height Int | Iterations Int | Seed Int | Edgehandling String | ShowHelp
options = [
    Option "w" ["width"] (ReqArg (parse Width) "WIDTH") "Width of the board",
    Option "h" ["height"] (ReqArg (parse Height) "HEIGHT") "Height of the board",
    Option "i" ["iterations"] (ReqArg (parse Iterations) "ITERS") "Number of iterations",
    Option "s" ["seed"] (ReqArg (parse Seed) "SEED") "Seed for the random number generator",
    Option "e" ["edgehandling"] (ReqArg ((\x->[x]) . Edgehandling) "[wrap|trunc]")
        "How to handle cells at the edge",
    Option "?" ["help"] (NoArg [ShowHelp]) "Show this output"
    ]

processOpts = foldl (\(w, h, loop, edge, seed) opt -> case opt of
        Width w' -> (w', h, loop, edge, seed)
        Height h' -> (w, h', loop, edge, seed)
        Iterations i -> (w, h, sequence_ . take i, edge, seed)
        Seed s -> (w, h, loop, edge, Just $ mkStdGen s)
        Edgehandling s -> case s of
            "wrap" -> (w, h, loop, wrapIdx, seed)
            "trunc" -> (w, h, loop, truncIdx, seed)
            _ -> error $ "Invalid edge handling mode: " ++ s
        ShowHelp -> error $ usageInfo "" options
    ) (50, 50, sequence_, wrapIdx, Nothing)

main = do
    args <- getArgs
    case getOpt Permute options args of
        (opts, _, []) -> showAutomaton . processOpts $ concat opts
        (_, _, errs) -> error $ concat errs

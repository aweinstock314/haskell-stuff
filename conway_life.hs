#!/usr/bin/env runghc
import Control.Exception
import Control.Monad
import Data.Array.IArray
--import Data.Array.IO
import Data.IORef
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Random
import qualified Data.Map as M

intOfBool True = 1
intOfBool False = 0

showCell True = putStr "#"
showCell False = putStr "_"

--makeRandomBoard arrayCtor gen (w, h) = arrayCtor ((0, 0), (w-1, h-1)) (map (`mod` 2) (randoms gen))

makeRandomBoard arrayCtor gen (w, h) density =
    arrayCtor ((0, 0), (w-1, h-1))
    (map (< density) (randoms gen :: [Double]))

dotimes = flip mapM_ . enumFromTo 0 . (\x -> x-1)

dogrid (w, h) eachIndex eachLine = do
    dotimes h (\ y -> do
        dotimes w (\ x -> do
            eachIndex x y
            )
        eachLine
        )

showBoard dim brd = dogrid dim (\x y -> showCell $ brd!(x, y)) (putStrLn "")
--showBoardMut dim brd = dogrid dim (\x y -> readArray brd (x, y) >>= showCell) (putStrLn "")

truncIdx (w, h) (x, y) = guard ((0 <= x) && (x < w) && (0 <= y) && (y < h)) >> return (x, y)
wrapIdx (w, h) (x, y) = Just (x `mod` w, y `mod` h)
adjacents (x, y) = do { dx <- [-1..1]; dy <- [-1..1]; delete (x, y) $ return (x+dx, y+dy) }

-- use the "unsafePerformIO hack" to make a cache
{-# NOINLINE adjacentsCache #-}
adjacentsCache :: IORef (M.Map (Int, Int) [(Int, Int)])
adjacentsCache = unsafePerformIO (newIORef $ M.fromList [])

cachedAdjacents (x, y) = unsafePerformIO $ do
    hm <- readIORef adjacentsCache
    case M.lookup (x, y) hm of
        Just adjs -> return adjs
        Nothing -> do
            let adjs = adjacents (x, y)
            writeIORef adjacentsCache $ M.insert (x, y) adjs hm
            return adjs

sumOfNeighbors handleEdges brd = sum . map (intOfBool . (brd!)) . catMaybes . map handleEdges . cachedAdjacents

amapi :: (IArray a e, IArray a e', Ix i) => (i -> e -> e') -> a i e -> a i e'
amapi f arr = array (minIx, maxIx) (map (\(i, e) -> (i, f i e)) (assocs arr)) where
    minIx = minimum $ indices arr
    maxIx = maximum $ indices arr

evolveBoard handleEdges brd = amapi (\(x, y) alive ->
    let neighborsAlive = sumOfNeighbors handleEdges brd (x, y) in
    -- A cell remains alive if it has 2 or 3 live neighbors, and a cell is born if it has 3 live neighbors
    if alive then (neighborsAlive `elem` [2, 3]) else (neighborsAlive == 3)
    ) brd

showAutomaton (w, h, loop, edge, seedgen) = do
    let linesBetweenIters = 1
    -- Set the buffering to dump a whole board to stdout at once, to make animation appear smooth(er)
    hSetBuffering stdout . BlockBuffering . Just $ (w+1)*h + linesBetweenIters
    gen <- fromMaybe getStdGen $ liftM return seedgen
    board <- newIORef (makeRandomBoard listArray gen (w, h) 0.3 :: Array (Int, Int) Bool)
    --board <- makeRandomBoard newListArray gen (w, h) :: IO (IOArray (Int, Int) Bool)
    loop $ do
        readIORef board >>= showBoard (w, h)
        replicateM_ linesBetweenIters $ putStrLn ""
        modifyIORef board $ evolveBoard (edge (w, h))

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
        Iterations i -> (w, h, replicateM_ i :: IO a -> IO (), edge, seed)
        Seed s -> (w, h, loop, edge, Just $ mkStdGen s)
        Edgehandling s -> case s of
            "wrap" -> (w, h, loop, wrapIdx, seed)
            "trunc" -> (w, h, loop, truncIdx, seed)
            _ -> error $ "Invalid edge handling mode: " ++ s
        ShowHelp -> error $ usageInfo "" options
    ) (50, 50, forever, wrapIdx, Nothing)

main = do
    args <- getArgs
    case getOpt Permute options args of
        (opts, _, []) -> showAutomaton . processOpts $ concat opts
        (_, _, errs) -> error $ concat errs

#!/usr/bin/env runghc
import Control.Exception
import Control.Monad
import Data.Array.IArray
--import Data.Array.IO
import Data.IORef
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Random

--makeRandomBoard gen (w, h) = listArray ((0, 0), (w-1, h-1)) (map (`mod` 2) (randoms gen))
--makeRandomBoard gen (w, h) = newListArray ((0, 0), (w-1, h-1)) (map (`mod` 2) (randoms gen))

makeRandomBoard gen (w, h) density =
    listArray ((0, 0), (w-1, h-1))
    (map (\x-> if x < density then 1 else 0) (randoms gen :: [Double]))

dotimes = flip mapM_ . enumFromTo 0 . (\x -> x-1)

dogrid (w, h) eachIndex eachLine = do
    dotimes h (\ y -> do
        dotimes w (\ x -> do
            eachIndex x y
            )
        eachLine
        )

showBoard dim brd = dogrid dim (\x y -> putStr . show $ brd!(x, y)) (putStrLn "")
--showBoard dim brd = dogrid dim (\x y -> readArray brd (x, y) >>= putStr . show) (putStrLn "")

neighbors brd x y =
    map (brd!) $ filter (\(x', y') -> (1 == max (abs (x-x')) (abs (y-y')))) (indices brd)

amapi :: (IArray a e, IArray a e', Ix i) => (i -> e -> e') -> a i e -> a i e'
amapi f arr = array (minIx, maxIx) (map (\(i, e) -> (i, f i e)) (assocs arr)) where
    minIx = minimum $ indices arr
    maxIx = maximum $ indices arr

evolveBoard brd = amapi (\(x, y) e ->
    let s = sum $ neighbors brd x y in
    case e of
        0 -> if s == 3 then 1 else 0
        1 -> if s `elem` [2, 3] then 1 else 0
        _ -> error "evolveBoard: Unexpected entry"
    ) brd

showAutomaton (w, h, loop) = do
    gen <- getStdGen
    --let board = makeRandomBoard gen (w, h) 0.3 :: Array (Int, Int) Int
    board <- newIORef (makeRandomBoard gen (w, h) 0.3 :: Array (Int, Int) Int)
    --board <- makeRandomBoard gen (w, h) :: IO (IOArray (Int, Int) Int)
    loop $ do
        readIORef board >>= showBoard (w, h)
        putStrLn ""
        modifyIORef board evolveBoard

parse ctor = catMaybes . map (Just . ctor . fst) . reads
data Opt = Width Int | Height Int | Iterations Int deriving (Eq, Show)
options = [
    Option "w" ["width"] (ReqArg (parse Width)  "WIDTH") "Width of the board",
    Option "h" ["height"] (ReqArg (parse Height) "HEIGHT") "Height of the board",
    Option "i" ["iterations"] (ReqArg (parse Iterations) "ITERS") "Number of iterations"
    ]
processOpts = foldr (\opt (w, h, loop) -> case opt of
        Width w' -> (w', h, loop)
        Height h' -> (w, h', loop)
        Iterations i -> (w, h, replicateM_ i :: IO a -> IO ())
    ) (50, 50, forever)

main = do
    args <- getArgs
    case getOpt Permute options args of
        (opts, _, []) -> showAutomaton . processOpts $ concat opts
        _ -> error $ usageInfo "" options

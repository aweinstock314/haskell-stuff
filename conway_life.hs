#!/usr/bin/env runghc
import Control.Monad
import Data.Array.IArray
--import Data.Array.IO
import Data.IORef
import System.Random

-- TODO: threshholds other than 50%
makeRandomBoard gen (w, h) = listArray ((0, 0), (w-1, h-1)) (map (`mod` 2) (randoms gen))
--makeRandomBoard gen (w, h) = newListArray ((0, 0), (w-1, h-1)) (map (`mod` 2) (randoms gen))

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

main = do
    let dim = (50, 50)
    gen <- getStdGen
    --let board = makeRandomBoard gen dim :: Array (Int, Int) Int
    board <- newIORef (makeRandomBoard gen dim :: Array (Int, Int) Int)
    --board <- makeRandomBoard gen (5, 5) :: IO (IOArray (Int, Int) Int)
    forever $ do
        readIORef board >>= showBoard dim
        putStrLn ""
        modifyIORef board evolveBoard

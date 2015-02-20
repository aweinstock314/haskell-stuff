#!/usr/bin/env runghc
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.IORef
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random

import ConwayLife

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

showAutomatonCPS (w, h, loop, edge, seedgen) = do
    setBoardBuffering w h
    gen <- fromMaybe getStdGen $ liftM return seedgen
    let initBoard = makeRandomBoard listArray gen (w, h) 0.3 :: UArray (Int, Int) Bool
    let boards = iterate (evolveBoardCPS (edge (w, h))) initBoard
    loop $ map (\brd -> showBoard (w, h) brd >> showSpacing) boards

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
            --"wrap" -> (w, h, loop, wrapIdxCPS, seed)
            --"trunc" -> (w, h, loop, truncIdxCPS, seed)
            _ -> error $ "Invalid edge handling mode: " ++ s
        ShowHelp -> error $ usageInfo "" options
    ) (50, 50, sequence_, wrapIdx, Nothing)
    --) (50, 50, sequence_, wrapIdxCPS, Nothing)

main = do
    args <- getArgs
    case getOpt Permute options args of
        (opts, _, []) -> showAutomaton . processOpts $ concat opts
        --(opts, _, []) -> showAutomatonCPS . processOpts $ concat opts
        (_, _, errs) -> error $ concat errs

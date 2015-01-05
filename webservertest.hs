#!/usr/bin/env runghc
import Control.Concurrent
import Control.Monad
import Network
import Network.HTTP.Base
import Network.HTTP.Stream
import System.IO

instance Stream Handle where
    readLine = liftM Right . (liftM $ flip (++) "\n") . hGetLine
    readBlock h i = liftM Right . replicateM i $ hGetChar h
    writeBlock h = liftM Right . hPutStr h
    close = hClose
    closeOnEnd h flag = error "Stream Handle => closeOnEnd: not implemented"

helloResponse = Response{rspCode=(2,0,0), rspReason="OK", rspHeaders=[], rspBody="Hello, world!"}

processRequest (handle, host, port) = do
    putStrLn $ "Accepted a connection from " ++ host ++ ":" ++ show port
    request <- receiveHTTP handle
    putStrLn $ "Received " ++ (show . show) request ++ " from " ++ host ++ ":" ++ show port
    respondHTTP handle helloResponse
    hClose handle

main = do
    listener <- listenOn (PortNumber 8101)
    forever $ accept listener >>= \t -> forkIO $ processRequest t

-- http://lpaste.net/146230
{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO

getc :: IO Word8
getc = alloca $ \c -> hGetBuf stdin c 1 >> peek c

gets :: Ptr Word8 -> IO ()
gets buf = do
    c <- getc
    poke buf c
    let next = (buf `plusPtr` 1)
    if c == fromIntegral (ord '\n')
        then poke next (0 :: Word8)
        else gets next

strlen :: Ptr Word8 -> IO Int
strlen buf = aux buf 0 where
    aux p !acc = peek p >>= \x -> case x of
        0 -> return acc
        _ -> aux (p `plusPtr` 1) (acc + 1)

puts :: Ptr Word8 -> IO ()
puts buf = do
    len <- strlen buf
    fmap (map (chr . fromIntegral)) (peekArray len buf) >>= putStr

main = allocaArray 128 $ \buf -> do
    hSetBuffering stdout NoBuffering
    putStr "Enter a thing to echo (no more than 128 bytes please ;) ) "
    gets buf
    putStr "Echoing: "
    puts buf

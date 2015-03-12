#!/usr/bin/env runhaskell
import Data.Char
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Unsafe.Coerce

-- This one's an infinite loop
hello1 :: String
hello1 =
    "\xeb\x1b\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca" ++
    "\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xe8\xe0\xff" ++
    "\xff\xffHello, world!\n"

-- This one exits after saying hello
hello2 :: String
hello2 =
    "\xeb\x1c\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca" ++
    "\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xf4\xe8\xdf" ++
    "\xff\xff\xffHello, world!\n"

-- This one contains null bytes, and exits after saying hello
hello3 :: String
hello3 =
    "\x48\xc7\xc2\x0e\x00\x00\x00\xb8\x01\x00\x00\x00\xbf\x02\x00" ++
    "\x00\x00\xeb\x04\x5e\x0f\x05\xf4\xe8\xf7\xff\xff\xffHello, w" ++
    "orld!\n"

-- Use Haskell's FFI to call a function pointer with the C calling convention. Documented by
--  https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/base-4.2.0.1/Foreign-Ptr.html#2
foreign import ccall "dynamic" callFunPtr :: FunPtr () -> IO ()

{-
Only works on machine code that has no null bytes:
unsafeExecuteString' :: String -> IO ()
unsafeExecuteString' str = useAsCString (pack str) (\ cstr -> unsafeCoerce cstr >>= callFunPtr)

Works perfectly, but isn't point-free:
unsafeExecuteString'' :: String -> IO ()
unsafeExecuteString'' str = do
    withArray (map (fromIntegral . ord) str :: [Word8]) $ \ptrArr -> do
        callFunPtr $ unsafeCoerce ptrArr
-}

unsafeExecuteString :: String -> IO ()
unsafeExecuteString = flip withArray (callFunPtr.unsafeCoerce) . map ((id::Word8->Word8).fromIntegral.ord)

main = unsafeExecuteString hello3

#!/usr/bin/env runghc
import Data.ByteString.Char8
import Foreign.Ptr
import Unsafe.Coerce

-- This one's an infinite loop
hello1 :: ByteString
hello1 = pack $
    "\xeb\x1b\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca" ++
    "\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xe8\xe0\xff" ++
    "\xff\xffHello, world!\n"

-- This one exits after saying hello
hello2 :: ByteString
hello2 = pack $
    "\xeb\x1c\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca" ++
    "\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xf4\xe8\xdf" ++
    "\xff\xff\xffHello, world!\n"

-- Use Haskell's FFI to call a function pointer with the C calling convention. Documented by
--  https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/base-4.2.0.1/Foreign-Ptr.html#2
foreign import ccall "dynamic" callFunPtr :: FunPtr () -> IO ()

unsafeExecuteByteString :: ByteString -> IO ()
unsafeExecuteByteString str = useAsCString str (\ cstr -> unsafeCoerce cstr  >>= callFunPtr)

main = unsafeExecuteByteString hello2

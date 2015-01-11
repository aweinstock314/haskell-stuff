{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Word
import qualified Foreign.C.Types
import qualified Foreign.Marshal
import qualified Foreign.Ptr
import qualified Unsafe.Coerce
import InlineAsm

{-
both "constant42Asm" and "identityAsm" seem to work under 
runghc, "factorialAsm" only seems to work when compiled...
-}

constant42Asm = bytesOfString "\xb0\x2a\xc3"
identityAsm = bytesOfString "\x89\xf8\xc3"
factorialAsm = bytesOfString "\xb0\x01\x89\xfb\x0f\xaf\xc3\xff\xcb\x83\xfb\x01\x75\xf6\xc3"

declareExecuteBytes "executeIntToInt" [t| Int -> Int |]

main = do
    const42 <- executeIntToInt constant42Asm
    identInt <- executeIntToInt identityAsm
    fact <- executeIntToInt factorialAsm
    print $ map ($ 5) [const42, identInt, fact]

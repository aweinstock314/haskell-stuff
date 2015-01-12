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
factorialAsm = bytesOfString "\xb0\x01\x89\xfb\x83\xfb\x01\x7c\x07\x0f\xaf\xc3\xff\xcb\xeb\xf4\xc3"

declareExecuteBytes "executeIntToInt" [t| Int -> Int |]

main = do
    const42 <- executeIntToInt constant42Asm
    identInt <- executeIntToInt identityAsm
    fact <- executeIntToInt factorialAsm
    print $ map (flip map [0..10]) [const42, identInt, fact]

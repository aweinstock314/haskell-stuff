{-# LANGUAGE TemplateHaskell #-}
module InlineAsm (declareExecuteBytes, bytesOfString) where
import Data.Char
import Data.Word
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Language.Haskell.TH
import Unsafe.Coerce

callFunPtrType fnType = [t| FunPtr $fnType -> $fnType |]

callFunPtrDecl name fnType = do
    cFPT <- callFunPtrType fnType
    return $ ForeignD (ImportF CCall Safe " dynamic" name cFPT)

executeBytesExp fnType cfpName = [| flip withArray (return . $cfpName . unsafeCoerce) :: [Word8] -> IO $fnType |]

declareExecuteBytes nameStr fnType = do
    let eBytesName = mkName nameStr
    cfpName <- newName "callFunPtr" -- TODO: type-based suffix
    cfpDecl <- callFunPtrDecl cfpName fnType
    eBytesExp <- executeBytesExp fnType (varE cfpName)
    let eBytesDecl = ValD (VarP eBytesName) (NormalB eBytesExp) []
    return [cfpDecl, eBytesDecl]

bytesOfString :: String -> [Word8]
bytesOfString = map (fromIntegral . ord)

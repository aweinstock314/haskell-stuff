{-# LANGUAGE TemplateHaskell #-}
module MkCached (mkCached) where
import Data.IORef
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import qualified Data.Map as M

{-
the interface for inline declarations changes between 
template-haskell-2.7.0.0 (debian's version) and 
template-haskell-2.9.0.0 (most current)
-}
declareNoInline :: Name -> Q Pragma
--declareNoInline name = InlineP name NoInline FunLike AllPhases
declareNoInline name = fmap (InlineP name) $ inlineSpecNoPhase False False

cacheCreationExp  = [| unsafePerformIO (newIORef $ M.fromList []) |]

cachedFnExp origFn cacheRef = [| (\x -> unsafePerformIO $ do {
    m <- readIORef $cacheRef;
    case M.lookup x m of {
        Just y -> return y;
        Nothing -> do {
            let y = $origFn x in
            (writeIORef $cacheRef $ M.insert x y m) >> return y
        }
    }
}) |]

mkCached :: String -> String -> DecsQ
mkCached fnStr cachedStr = do
    maybeFnName <- lookupValueName fnStr
    let fnName = case maybeFnName of {
        Just name -> name;
        Nothing -> error $ "Name given (\"" ++ fnStr ++ "\") doesn't exist in the value namespace."
    }
    -- newName gensyms, but suffix the name anyway, for -ddump-splices output
    cacheName <- newName $ fnStr ++ "Cache"
    noInlinePragma <- declareNoInline cacheName
    createCache <- cacheCreationExp
    cachedFn <- cachedFnExp (varE fnName) (varE cacheName)
    let createCacheDecl = FunD cacheName [Clause [] (NormalB createCache) []]
    let cachedFnDecl = FunD (mkName cachedStr) [Clause [] (NormalB cachedFn) []]
    {-
    -- These seem to work too, but cause the compiled program to run slower?...
    let createCacheDecl = ValD (VarP cacheName) (NormalB createCache) []
    let cachedFnDecl = ValD (VarP (mkName cachedStr)) (NormalB cachedFn) []
    -}
    return [PragmaD noInlinePragma, createCacheDecl, cachedFnDecl]

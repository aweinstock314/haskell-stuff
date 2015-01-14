{-# LANGUAGE TemplateHaskell #-}
module MkCached (mkCached, mkCachedTyped, mkCachedAutoTyped) where
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

cacheCreationExp (Just ty) = [| unsafePerformIO . newIORef $ (M.empty :: $ty) |]
cacheCreationExp Nothing = [| unsafePerformIO . newIORef $ M.empty |]

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

mkCached fnStr cachedStr = mkCachedPossiblyTyped fnStr cachedStr (Left False)
mkCachedTyped fnStr cachedStr ty = mkCachedPossiblyTyped fnStr cachedStr (Right ty)
mkCachedAutoTyped fnStr cachedStr = mkCachedPossiblyTyped fnStr cachedStr (Left True)

substituteTypes (AppT (AppT ArrowT domain) codomain) = return (AppT (AppT (ConT (mkName "Data.Map.Map")) domain) codomain)
substituteTypes _ = error $ "Handling of types other than 2-argument functions isn't implemented."

mkCachedPossiblyTyped :: String -> String -> Either Bool TypeQ -> DecsQ
mkCachedPossiblyTyped fnStr cachedStr maybeTypeDecl = do
    maybeFnName <- lookupValueName fnStr
    let fnName = case maybeFnName of {
        Just name -> name;
        Nothing -> error $ "Name given (\"" ++ fnStr ++ "\") doesn't exist in the value namespace."
    }
    fnInfo <- reify fnName
    let fnType = case fnInfo of {
        VarI _ ty _ _ -> ty;
        _ -> error $ "Unable to dissect type."
    }
    {-
    runIO $ do
        --print fnName
        let withTypeDecl f = either (return . const ()) ((>>= f) . runQ) maybeTypeDecl
        putStrLn $ pprint fnType
        print fnType
        putStrLn ""
        withTypeDecl $ putStrLn . pprint
        withTypeDecl print
    -}
    -- newName gensyms, but suffix the name anyway, for -ddump-splices output
    cacheName <- newName $ fnStr ++ "Cache"
    noInlinePragma <- declareNoInline cacheName
    let cacheType = case maybeTypeDecl of {
        Left True -> (Just $ substituteTypes fnType);
        Left False -> Nothing;
        Right ty -> Just ty
    }
    createCache <- cacheCreationExp cacheType
    cachedFn <- cachedFnExp (varE fnName) (varE cacheName)
    let createCacheDecl = FunD cacheName [Clause [] (NormalB createCache) []]
    let cachedFnDecl = FunD (mkName cachedStr) [Clause [] (NormalB cachedFn) []]
    {-
    -- These seem to work too, but cause the compiled program to run slower?...
    let createCacheDecl = ValD (VarP cacheName) (NormalB createCache) []
    let cachedFnDecl = ValD (VarP (mkName cachedStr)) (NormalB cachedFn) []
    -}
    return [PragmaD noInlinePragma, createCacheDecl, cachedFnDecl]

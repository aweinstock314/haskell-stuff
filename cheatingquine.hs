{-# LANGUAGE TemplateHaskell, CPP #-}
import Language.Haskell.TH
{- Note: this reads the source at compile time, not at 
runtime; so if the source is modified after compiling, the 
compiled version outputs the unmodified source (until it's 
recompiled) -}
sourceCode = $(return (readFile __FILE__) >>= runIO >>= stringE)
main = putStr sourceCode

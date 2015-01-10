-- strings ./cheatingquine | grep -A $(./cheatingquine | wc -l) 'grep -A'
{-# LANGUAGE TemplateHaskell, CPP #-}
import Language.Haskell.TH
#define fileLiteral (runIO . fmap (LitE . StringL) . readFile)
{- Note: this reads the source at compile time, not at 
runtime; so if the source is modified after compiling, the 
compiled version outputs the unmodified source (until it's 
recompiled) -}
sourceCode = $(fileLiteral __FILE__)
main = putStr sourceCode

#!/bin/sh
TMP=$(tempfile)
echo "import Data.Char; main = putStrLn . unlines $ map ((\".byte \" ++) . show . ord) \"$1\"" | runghc | as -o $TMP && objdump -d $TMP
rm $TMP

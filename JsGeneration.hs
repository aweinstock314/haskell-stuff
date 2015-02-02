--{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module JsGeneration where
import Data.List
import Data.Monoid
import Language.Javascript.JMacro

{-
data JS a where
    Lit :: a -> JS a
    Var :: String -> JS a
    BinOp :: String -> JS a -> JS b -> JS c
    Assign :: JS a -> JS b -> JS ()
    For :: (JS a, JS Bool, JS c) -> JS d -> JS () 
    -- | Lam ::([String] -> JS)
-}

data JS =
    Lit String
    | Var String
    | BinOp String (JS) (JS)
    | For (JS, JS, JS) [JS]
    | Call JS [JS]
    | Lam [String] ([JS] -> [JS])
    | Return JS
    | Method String JS [JS]

lit :: Show a => a -> JS
lit = Lit . show

sequencify :: Show a => [a] -> String
sequencify = mconcat . map ((++";") . show)

call = Call . Var

instance Show JS where
    show (Lit x) = x
    show (Var name) = name
    show (BinOp op l r) = mconcat [show l, op, show r]
    show (For (init, cond, step) body) = mconcat [mconcat ["for(", show init, ";", show cond, ";", show step, "){"], sequencify body, "}"]
    show (Call name args) = mconcat $ mconcat [[show name], ["("], intersperse "," $ map show args, [")"]]
    show (Lam args generator) = mconcat $ mconcat [["function("], intersperse ", " args, ["){"], [sequencify . generator $ map Var args], ["}"]]
    show (Return x) = mconcat ["return ", show x]
    show (Method m a b) = mconcat [show a, ".", m, "(", mconcat . intersperse ", " $ map show b, ")"]

assign = BinOp "="
increment x = BinOp "+=" x (lit 1)
decrement x = BinOp "+=" x (lit (-1))

jsFactorial = let (res, i) = (Var "res", Var "i") in
    assign (Var "factorial") $ Lam ["n"] $ \[n] -> [
        assign res (lit 1),
        For (assign i n, BinOp ">" i (lit 0), decrement i) $ [
            assign res (BinOp "*" res i)
        ],
        Return res
    ]

jsUsesFactorial = sequencify $ [jsFactorial] ++ map (call "alert" . return . call "factorial" . return . lit) [1..10]
jsUsesFactorial2 = sequencify $ [jsFactorial, Method "map" (lit [1..10]) [Lam ["n"] (\[n] -> [call "alert" [call "factorial" [n]]])]]

jsFactorial' = [jmacro|factorial = function(n){var res=1;for(var i=n;i>0;i--){res*=i;}return res;}|]
jsUsesFactorial' = renderJs $ jsFactorial' <> [jmacro| `[1..10]::[Int]`.map(\x->alert(factorial(x)))|]

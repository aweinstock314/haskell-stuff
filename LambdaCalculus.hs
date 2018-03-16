import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import Debug.Trace

import PA1Helper (Lexp(..), runProgram)
-- http://www.cs.rpi.edu/academics/courses/fall17/proglang/pa1/PA1Helper.hs
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp Lexp
-- runProgram :: FilePath -> (Lexp -> Lexp) -> IO ()

import System.Environment (getArgs)
import Test.QuickCheck
import qualified Data.Set as S

alphabetical = elements ['a'..'z']
variable scope = frequency [
    (4, onechar),
    (5, if null scope then onechar else oneof (map pure scope)),
    (1, (:) <$> alphabetical <*> variable [])
    ] where onechar = (:[]) <$> alphabetical
lexp scope = frequency [
            (5, Atom <$> variable scope),
            (3, do { v <- variable scope; e <- lexp (v:scope); return (Lambda v e) }),
            (2, Apply <$> lexp scope <*> lexp scope)
        ]

instance Arbitrary Lexp where
    arbitrary = lexp []
    --shrink = genericShrink

-- TAPL 5.2.3 / https://en.wikipedia.org/wiki/Lambda_calculus#Free_variables
freevars (Atom x) = S.singleton x
freevars (Lambda x e) = freevars e S.\\ S.singleton x
freevars (Apply e1 e2) = freevars e1 `S.union` freevars e2

-- Original
increment name = takeWhile (not . isNum) name ++ (show (1 + getNumericSuffix name)) where
    isNum = (`elem` ['0'..'9'])
    reads' x = case reads x of { [] -> 0; ((y,_):_) -> y }
    getNumericSuffix = reads' . takeWhile isNum . dropWhile (not . isNum)

while p f x = if p x then while p f (f x) else x

--gensym name avoid | name `S.member` avoid = gensym (increment name) avoid where
--gensym name avoid = name
gensym name avoid = while (`S.member` avoid) increment name

-- Adapted from TAPL 5.3.5
subst x s (Atom y) | x == y = s
subst x s t@(Atom _) = t
subst x s t@(Lambda y _) | x == y = t
subst x s (Lambda y e) | x /= y && not (y `S.member` freevars s) = Lambda y (subst x s e)
subst x s t@(Lambda y e) | x /= y && (y `S.member` freevars s) = let y' = gensym y (freevars t `S.union` freevars s) in (Lambda y' (subst x s (subst y (Atom y') e)))
subst x s (Apply e1 e2) = (Apply `on` subst x s) e1 e2

-- TAPL 5-3
--beta x | trace (show $ show x) False = undefined
beta (Apply (Lambda x e1) e2) = let e2' = beta e2 in e2' `seq` (subst x e2' e1) -- E-AppAbs
beta (Atom x) = Atom x
beta (Lambda x e) = Lambda x (beta e)
beta (Apply e1 e2) = Apply (beta e1) (beta e2) -- E-App1 (should we add E-App2?)
{- TODO: should (x \y.(\z.z y)) beta-reduce to (x \y.y) ?
beta (Apply (Atom "x") (Lambda "y" (Apply (Lambda "z" (Atom "z")) (Atom "y"))))
-}
constOmegaExample = fixConverge (eta . beta) (Apply (Lambda "x" (Atom "y")) (let f = Lambda "x" (Apply (Atom "x") (Atom "x")) in Apply f f))

eta (Lambda x (Apply e (Atom y))) | x == y && (not (x `S.member` freevars e)) = e
eta (Atom x) = Atom x
eta (Lambda x e) = Lambda x (eta e)
eta (Apply e1 e2) = Apply (eta e1) (eta e2)

data DeBruijn = Atom' Int | Lambda' DeBruijn | Apply' DeBruijn DeBruijn deriving (Eq, Show)

instance Arbitrary DeBruijn where
    arbitrary = fmap (fst . removeNames) arbitrary

-- TAPL 6.1.5
removeNames :: Lexp -> (DeBruijn, [String])
removeNames exp = (aux startGamma exp, startGamma) where
    startGamma = S.toList (freevars exp)
    aux gamma (Atom x) = Atom' (fromJust (findIndex (==x) gamma))
    aux gamma (Lambda x e) = Lambda' (aux (x:gamma) e)
    aux gamma (Apply e1 e2) = (Apply' `on` aux gamma) e1 e2 -- TAPL may have a bug?

restoreNames :: (DeBruijn, [String]) -> Lexp
restoreNames (exp, gamma) = aux gamma exp where
    aux gamma (Atom' i) = Atom (gamma !! i)
    aux gamma (Lambda' e) = let x = gensym "x" (S.fromList gamma) in Lambda x (aux (x:gamma) e)
    aux gamma (Apply' e1 e2) = (Apply `on` aux gamma) e1 e2 -- TAPL may have a bug?

-- TAPL 6.2.1
shift d c (Atom' k) | k < c = Atom' k
shift d c (Atom' k) | k >= c = Atom' (k+d)
shift d c (Lambda' t1) = Lambda' (shift d (c+1) t1)
shift d c (Apply' t1 t2) = Apply' (shift d c t1) (shift d c t2)

-- TAPL 6.2.4
debruijnSubst j s (Atom' k) | k == j = s
debruijnSubst j s (Atom' k) | k /= j = (Atom' k)
debruijnSubst j s (Lambda' t1) = Lambda' (debruijnSubst (j+1) (shift 1 0 s) t1)
debruijnSubst j s (Apply' t1 t2) = (Apply' `on` debruijnSubst j s) t1 t2

-- Adapted from TAPL 7.3
debruijnBeta (Apply' (Lambda' t1) t2) = let t2' = debruijnBeta t2 in t2' `seq` shift (-1) 0 (debruijnSubst 0 (shift 1 0 t2') t1)
debruijnBeta t@(Atom' _) = t
debruijnBeta (Lambda' t) = Lambda' (debruijnBeta t)
debruijnBeta (Apply' t1 t2) = Apply' (debruijnBeta t1) (debruijnBeta t2)

alphaEquivalent e1 e2 = removeNames e1 == removeNames e2



data SK = S | K | Free String | App SK SK deriving (Eq, Show)

combinatorI = App (App S K) K

freevarsSK S = S.empty
freevarsSK K = S.empty
freevarsSK (Free x) = S.singleton x
freevarsSK (App e1 e2) = (S.union `on` freevarsSK) e1 e2

-- https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis
translateSK (Atom x) = Free x
translateSK (Apply e1 e2) = App (translateSK e1) (translateSK e2)
translateSK (Lambda x e) | not (x `S.member` freevars e) = App K (translateSK e)
translateSK (Lambda x (Atom y)) | x == y = combinatorI
translateSK (Lambda x (Lambda y e)) | x `S.member` freevars e = translateSK (Lambda x (untranslateSK (translateSK (Lambda y e))))
translateSK (Lambda x (Apply e1 e2)) | x `S.member` (S.union `on` freevars) e1 e2 = App (App S (translateSK (Lambda x e1))) (translateSK (Lambda x e2))

untranslateSK S = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Atom "x") (Atom "z")) (Apply (Atom "y") (Atom "z")))))
untranslateSK K = Lambda "x" (Lambda "y" (Atom "x"))
untranslateSK (Free x) = Atom x
untranslateSK (App e1 e2) = (Apply `on` untranslateSK) e1 e2

evalSK (App (App K x) y) = x
evalSK (App (App (App S x) y) z) = App (App x z) (App y z)
evalSK e = e

{-
*Main> (fixConverge evalSK) (App combinatorI (Free "x"))
Free "x"
-}

churchExpr n = Lambda "f" (Lambda "x" (aux n)) where
    aux 0 = Atom "x"
    aux n = Apply (Atom "f") (aux (n-1))

churchPlus = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Atom "m") (Atom "f")) (Apply (Apply (Atom "n") (Atom "f")) (Atom "x"))))))
--let nine = Apply (Apply churchPlus (churchExpr 5)) (churchExpr 4)
applyToFx lexp = Apply (Apply lexp (Atom "f")) (Atom "x")

alphaEquivalenceTests = [
    not $ alphaEquivalent (Atom "x") (Atom "y"),
    alphaEquivalent (Atom "x") (Atom "x"),
    alphaEquivalent (Lambda "x" (Atom "x")) (Lambda "y" (Atom "y")),
    not $ alphaEquivalent (Lambda "x" (Atom "y")) (Lambda "y" (Atom "y")),
    alphaEquivalent (Lambda "x" (Atom "y")) (Lambda "z" (Atom "y"))
    ]

tests = do
    quickCheck (\x -> (restoreNames . removeNames) x `alphaEquivalent` x)
    quickCheck (\x -> (fst . removeNames . restoreNames) (x, fmap (:[]) ['a'..'z']) == x)
    quickCheck (\n m -> (n >= 0 && m >= 0) ==> eval (applyToFx $ Apply (Apply churchPlus (churchExpr (n :: Int))) (churchExpr m)) == eval (applyToFx $ churchExpr (n+m)))
    quickCheck (\lexp -> (restoreNames . (\(deb, ctx) -> (debruijnBeta deb, ctx)) . removeNames) lexp `alphaEquivalent` beta lexp)
    --quickCheck (\lexp -> (eval . untranslateSK . fixConverge evalSK . translateSK) lexp `alphaEquivalent` eval lexp) -- sometimes nonterminates
    --sample (fmap (\x -> restoreNames (x, take 10 (iterate increment "y"))) arbitrary)
    putStrLn $ if and alphaEquivalenceTests then "AlphaEq passed" else "AlphaEq failed"

fixConverge f x = let ys = iterate f x in fst (fromJust (find (uncurry (==)) (zip ys (tail ys))))
betaDB = restoreNames . (\(deb, ctx) -> (debruijnBeta deb, ctx)) . removeNames
eval = fixConverge (eta . beta)
eval' = fixConverge (eta . betaDB)

main = do
    args <- getArgs
    let filename = case args of { [x] -> x; _ -> "input.lambda" }
    --runProgram filename (eta . beta)
    runProgram filename eval'

#!/usr/bin/env runhaskell
import Data.List
import Text.Printf

data Expr =
      Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Matrix [[Expr]]
    | Var String
    | Lit Double
    deriving (Eq)

instance Show Expr where
    show (Add e1 e2) = printf "(%s)+(%s)" (show e1) (show e2)
    show (Mul e1 e2) = printf "(%s)*(%s)" (show e1) (show e2)
    show (Neg e) = printf "(-%s)" (show e)
    show (Matrix m) = printf "(Matrix %s)" $ show m
    show (Var name) = name
    show (Lit x) = show x

mat3x3 ((a11, a12, a13),
        (a21, a22, a23),
        (a31, a32, a33)) = Matrix $
       [[a11, a12, a13],
        [a21, a22, a23],
        [a31, a32, a33]]

matrixMultiply (Matrix a) (Matrix b) = Matrix $ map' ((foldl1' Add .) . zipWith Mul) a (transpose b)
    where map' f a b = map (\x -> map (\y -> f x y) b) a

simplify (Add (Lit x) (Lit y)) = Lit (x + y)
simplify (Mul (Lit x) (Lit y)) = Lit (x * y)

simplify (Add (Lit 0) x) = simplify x
simplify (Add x (Lit 0)) = simplify x
simplify (Add (Neg x) y) | x == y = Lit 0
simplify (Add x (Neg y)) | x == y = Lit 0

simplify (Add (Add x y) z) = simplify (Add x (Add y z))

simplify (Mul (Lit 0) _) = Lit 0
simplify (Mul _ (Lit 0)) = Lit 0

simplify (Mul (Lit 1) x) = simplify x
simplify (Mul x (Lit 1)) = simplify x

simplify (Mul (Lit (-1)) x) = Neg (simplify x)
simplify (Mul x (Lit (-1))) = Neg (simplify x)

simplify (Neg (Neg x)) = simplify x
simplify (Neg (Lit x)) = Lit (negate x)

simplify (Add x y) = Add (simplify x) (simplify y)
simplify (Mul x y) = Mul (simplify x) (simplify y)
simplify (Neg x) = Neg (simplify x)

simplify (Matrix m) = Matrix $ map (map simplify) m

simplify x = x

expr1 = ((Lit 2) `Mul` (Var "a") `Add` (Lit 0)) `Mul` (Lit 1)

expr2 = matrixMultiply (mat3x3 ((Var "A", Var "B", Var "C"),
                                (Var "D", Var "E", Var "F"),
                                (Var "G", Var "H", Var "I")))
                       (mat3x3 ((Var "R", Var "S", Var "T"),
                                (Var "U", Var "V", Var "W"),
                                (Var "X", Var "Y", Var "Z")))

expr3 = let (sin_t, cos_t) = (Var "sin(t)", Var "cos(t)") in
    matrixMultiply (mat3x3 ((Var "a", Lit   0, Var "x"),
                            (Var "c", Var "d", Var "y"),
                            (Lit   0, Lit   0, Lit   1)))
                   (mat3x3 ((cos_t, Neg sin_t, Lit 0),
                            (sin_t,     cos_t, Lit 0),
                            (Lit 0,     Lit 0, Lit 1)))

-- https://www.opengl.org/sdk/docs/man2/xhtml/glRotate.xml
rotation c s x y z = Matrix [
    [x*x*(one - c) + c, x*y*(one - c) - (z*s), x*z*(one - c) - (y*s), zero],
    [y*x*(one - c) + (z*s), y*y*(one - c) + c, y*z*(one - c) - (x*s), zero],
    [x*z*(one - c) - (y*s), y*z*(one - c) + (x*s), z*z*(one - c) + c, zero],
    [zero, zero, zero, one]] where
        (zero, one) = (Lit 0, Lit 1)
        ((+),(*)) = (Add, Mul)
        a - b = a `Add` (Neg b)

iterateToConvergence f init = unfoldr aux Nothing where
    aux Nothing = mkState init
    aux (Just prev) = if f prev == prev then Nothing else mkState $ f prev
    mkState x = Just (x, Just x)

showDerivation :: Show a => [a] -> IO ()
showDerivation = mapM_ (\x -> print x >> putStrLn "")

main = showDerivation $ iterateToConvergence simplify expr3

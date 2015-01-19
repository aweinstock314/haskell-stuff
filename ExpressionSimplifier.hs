#!/usr/bin/env runghc
import Data.List
import Text.Printf

data Expr =
      Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Mat3x3 ((Expr, Expr, Expr), (Expr, Expr, Expr), (Expr, Expr, Expr))
    | Var String
    | Lit Double
    deriving (Eq)

instance Show Expr where
    show (Add e1 e2) = printf "(%s)+(%s)" (show e1) (show e2)
    show (Mul e1 e2) = printf "(%s)*(%s)" (show e1) (show e2)
    show (Neg e) = printf "(-%s)" (show e)
    show (Mat3x3 ((a11, a12, a13),
                  (a21, a22, a23),
                  (a31, a32, a33))) = printf "(Mat3x3 [[%s, %s, %s], [%s, %s, %s], [%s, %s, %s]])"
        (show a11) (show a12) (show a13)
        (show a21) (show a22) (show a23)
        (show a31) (show a32) (show a33)
    show (Var name) = name
    show (Lit x) = show x

-- TODO: simplify and generalize matrix handling
matrixMultiply (Mat3x3 ((a11, a12, a13),
                        (a21, a22, a23),
                        (a31, a32, a33)))
               (Mat3x3 ((b11, b12, b13),
                        (b21, b22, b23),
                        (b31, b32, b33))) =
    Mat3x3 $ let ((+), (*)) = (Add, Mul) in
    ((((a11*b11)+(a12*b21)+(a13*b31)), ((a11*b12)+(a12*b22)+(a13*b32)), ((a11*b13)+(a12*b23)+(a13*b33))),
     (((a21*b11)+(a22*b21)+(a23*b31)), ((a21*b12)+(a22*b22)+(a23*b32)), ((a21*b13)+(a22*b23)+(a23*b33))),
     (((a31*b11)+(a32*b21)+(a33*b31)), ((a31*b12)+(a32*b22)+(a33*b32)), ((a31*b13)+(a32*b23)+(a33*b33))))

simplify (Add (Lit 0) x) = simplify x
simplify (Add x (Lit 0)) = simplify x

simplify (Mul (Lit 0) _) = Lit 0
simplify (Mul _ (Lit 0)) = Lit 0

simplify (Mul (Lit 1) x) = simplify x
simplify (Mul x (Lit 1)) = simplify x

simplify (Neg (Neg x)) = simplify x

simplify (Add x y) = Add (simplify x) (simplify y)
simplify (Mul x y) = Mul (simplify x) (simplify y)
simplify (Neg x) = Neg (simplify x)

simplify (Mat3x3 ((a11, a12, a13),
                  (a21, a22, a23),
                  (a31, a32, a33))) =
         (Mat3x3 ((simplify a11, simplify a12, simplify a13),
                  (simplify a21, simplify a22, simplify a23),
                  (simplify a31, simplify a32, simplify a33)))

simplify x = x

expr1 = ((Lit 2) `Mul` (Var "a") `Add` (Lit 0)) `Mul` (Lit 1)

expr2 = matrixMultiply (Mat3x3 ((Var "A", Var "B", Var "C"),
                                (Var "D", Var "E", Var "F"),
                                (Var "G", Var "H", Var "I")))
                       (Mat3x3 ((Var "R", Var "S", Var "T"),
                                (Var "U", Var "V", Var "W"),
                                (Var "X", Var "Y", Var "Z")))

expr3 = let (sin_t, cos_t) = (Var "sin(t)", Var "cos(t)") in
    matrixMultiply (Mat3x3 ((Var "a", Lit   0, Var "x"),
                            (Var "c", Var "d", Var "y"),
                            (Lit   0, Lit   0, Lit   1)))
                   (Mat3x3 ((cos_t, Neg sin_t, Lit 0),
                            (sin_t,     cos_t, Lit 0),
                            (Lit 0,     Lit 0, Lit 1)))

iterateToConvergence f init = unfoldr aux Nothing where
    aux Nothing = mkState init
    aux (Just prev) = if f prev == prev then Nothing else mkState $ f prev
    mkState x = Just (x, Just x)

showDerivation :: Show a => [a] -> IO ()
showDerivation = mapM_ (\x -> print x >> putStrLn "")

main = showDerivation $ iterateToConvergence simplify expr3

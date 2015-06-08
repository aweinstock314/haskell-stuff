#!/usr/bin/env runhaskell
import Data.List
import Text.Printf

data Expr =
      Add Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Neg Expr
    | Matrix [[Expr]]
    | Var String
    | Lit Double
    deriving (Eq)

instance Show Expr where
    show (Add e1 e2) = printf "(%s)+(%s)" (show e1) (show e2)
    show (Mul e1 e2) = printf "(%s)*(%s)" (show e1) (show e2)
    show (Div e1 e2) = printf "(%s)/(%s)" (show e1) (show e2)
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

newtype GLSL = GLSL Expr

instance Show GLSL where
    showsPrec p (GLSL (Add x y)) = showParen (p > 6) $ showsPrec 6 (GLSL x) . ('+':) . showsPrec 6 (GLSL y)
    showsPrec p (GLSL (Mul x y)) = showParen (p > 7) $ showsPrec 7 (GLSL x) . ('*':) . showsPrec 7 (GLSL y)
    showsPrec p (GLSL (Div x y)) = showParen (p > 7) $ showsPrec 7 (GLSL x) . ('/':) . showsPrec 7 (GLSL y)
    showsPrec p (GLSL (Neg x)) = showParen (p > 10) $ ('-':) . showsPrec p (GLSL x)
    -- GLSL matrices are 4x4, column-major
    showsPrec p (GLSL (Matrix [[a11, a12, a13, a14],
                               [a21, a22, a23, a24],
                               [a31, a32, a33, a34],
                               [a41, a42, a43, a44]])) = ("mat4"++) .
        showParen True (foldr (.) id . intersperse (',':) . map shows $
                            [a11, a21, a31, a41,
                             a12, a22, a32, a42,
                             a13, a23, a33, a43,
                             a14, a24, a34, a44])
    showsPrec p (GLSL (Matrix _)) = error "Non-4x4 matrices are currently unsupported for GLSL output"
    showsPrec p (GLSL (Var name)) = showString name
    showsPrec p (GLSL (Lit x)) = showsPrec 0 x

matrixMultiply (Matrix a) (Matrix b) = Matrix $ map' ((foldl1' Add .) . zipWith Mul) a (transpose b)
    where map' f a b = map (\x -> map (\y -> f x y) b) a

simplify (Add (Lit x) (Lit y)) = Lit (x + y)
simplify (Mul (Lit x) (Lit y)) = Lit (x * y)
simplify (Div (Lit x) (Lit y)) = Lit (x / y)
simplify (Neg (Lit x)) = Lit (negate x)

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

simplify (Add x y) = Add (simplify x) (simplify y)
simplify (Mul x y) = Mul (simplify x) (simplify y)
simplify (Div x y) = Div (simplify x) (simplify y)
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
    [x*x*(one - c) + c, x*y*(one - c) - (z*s), x*z*(one - c) + (y*s), zero],
    [y*x*(one - c) + (z*s), y*y*(one - c) + c, y*z*(one - c) - (x*s), zero],
    [x*z*(one - c) - (y*s), y*z*(one - c) + (x*s), z*z*(one - c) + c, zero],
    [zero, zero, zero, one]] where
        (zero, one) = (Lit 0, Lit 1)
        ((+),(*)) = (Add, Mul)
        a - b = a `Add` (Neg b)

-- https://www.opengl.org/sdk/docs/man2/xhtml/glTranslate.xml
translation x y z = Matrix [
    [ one, zero, zero,   x],
    [zero,  one, zero,   y],
    [zero, zero,  one,   z],
    [zero, zero, zero, one]] where
        (zero, one) = (Lit 0, Lit 1)

-- https://www.opengl.org/sdk/docs/man2/xhtml/glFrustum.xml
frustum left right bottom top near far = Matrix [
    [(two*near)/(right-left), zero, (right+left)/(right-left), zero],
    [zero, (two*near)/(top-bottom), (top+bottom)/(top-bottom), zero],
    [zero, zero, Neg ((far+near)/(far-near)), (Neg $ two*far*near)/(far-near)],
    [zero, zero, Lit (-1), zero]] where
        (zero, two) = (Lit 0, Lit 2)
        ((+),(*),(/)) = (Add, Mul, Div)
        a - b = a `Add` (Neg b)

expr4 = foldl1' matrixMultiply [
    frustum (Lit (-1)) (Lit 1) (Lit (-1)) (Lit 1) (Lit 0.5) (Lit 100),
    rotation (Var "cos(cameraOri.y)") (Var "sin(cameraOri.y)") (Lit (-1)) (Lit 0) (Lit 0),
    rotation (Var "cos(cameraOri.x)") (Var "sin(cameraOri.x)") (Lit 0) (Lit 1) (Lit 0),
    translation (Neg $ Var "cameraPos.x") (Neg $ Var "cameraPos.y") (Var "cameraPos.z"),
    translation (Var "objectPos.x") (Var "objectPos.y") (Neg $ Var "objectPos.z"),
    rotation (Var "cos(objectOri.x)") (Var "sin(objectOri.x)") (Lit 0) (Lit (-1)) (Lit 0),
    rotation (Var "cos(objectOri.y)") (Var "sin(objectOri.y)") (Lit 1) (Lit 0) (Lit 0)
    ]


iterateToConvergence f init = unfoldr aux Nothing where
    aux Nothing = mkState init
    aux (Just prev) = if f prev == prev then Nothing else mkState $ f prev
    mkState x = Just (x, Just x)

simplify' = last . iterateToConvergence simplify

showDerivation :: Show a => [a] -> IO ()
showDerivation = mapM_ (\x -> print x >> putStrLn "")

main = showDerivation . map GLSL $ iterateToConvergence simplify expr4

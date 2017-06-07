{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Data.Ord
import Data.Word
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck

littleEndianComposition = foldr (\x k -> 2*k+if x then 1 else 0) 0
littleEndianDecomposition 0 = []
littleEndianDecomposition n = case n `divMod` 2 of (n', bit) -> (bit == 1) : littleEndianDecomposition n'

pad xs ys val = aux xs ys where
    aux [] [] = ([])
    aux xs [] = zip xs (replicate (length xs) val)
    aux [] ys = zip (replicate (length ys) val) ys
    aux (x : xs) (y : ys) = (x, y) : aux xs ys

data Gate = And | Xor deriving Show
data Circuit a = Input a | Constant Bool | Not (Circuit a) | Function Gate (Circuit a) (Circuit a) | Tagged String (Circuit a) deriving Show

cAnd x y = Function And x y
cXor x y = Function Xor x y
cOr x y = Tagged "||" $ Not (cAnd (Not x) (Not y))
cEq x y = Tagged "==" $ Not (cXor x y)

evalG And = (&&)
evalG Xor = (/=)

evalC :: (a -> Bool) -> Circuit a -> Bool
evalC env (Input s) = env s
evalC env (Constant b) = b
evalC env (Not c) = not (evalC env c)
evalC env (Function g c1 c2) = evalG g (evalC env c1) (evalC env c2)
evalC env (Tagged s c) = evalC env c

ltBit x y = Tagged "<" $ Not x `cAnd` y

-- a < b (as unsigned little-endian words) iff exists i such that the i'th bit a is less than the i'th bit of b and all the leading bits (j > i) are equal
-- `foldl1 cOr` is the binder for the exists, `foldl1 cAnd` is the binder for the forall (mixed with the and to avoid the 0 edge-case)
ltUnsigned :: [(Circuit a, Circuit a)] -> Circuit a
--ltUnsigned inputs = foldl1 cOr [foldl1 ((Tagged "&&" .) . cAnd) (ltBit xi yi : [uncurry cEq (inputs !! j) | j <- [i+1..length inputs-1]]) | (i, (xi, yi)) <- zip [0..] inputs]
--ltUnsigned inputs = foldl1 cOr $ do { (i, (xi, yi)) <- zip [0..] inputs; [foldl1 ((Tagged "&&" .) . cAnd) (ltBit xi yi : do { j <- [i+1..length inputs-1]; return $ uncurry cEq (inputs !! j) })]}
ltUnsigned inputs = foldl1 cOr $ do { (i, (xi, yi)) <- zip [0..] inputs; [foldl1 ((Tagged "&&" .) . cAnd) (ltBit xi yi : map (uncurry cEq) (drop (i+1) inputs))]}

ltUnsignedDirect x y = foldr1 (||) [foldr1 (&&) ((xi < yi) : [uncurry (==) (inputs !! j) | j <- [i+1..length inputs-1]]) | (i, (xi, yi)) <- zip [0..] inputs] where
    inputs = pad (littleEndianDecomposition x) (littleEndianDecomposition y) False

halfAdder x y z = (sum, carry) where
    sum = Tagged "Sum" $ foldl1 cXor [x,y,z]
    carry = Tagged "Carry" $ foldl1 cOr [x `cAnd` y, y `cAnd` z, x `cAnd` z]

adder :: [(Circuit a, Circuit a)] -> [Circuit a]
adder =
    (\(outputs, carry) -> reverse outputs ++ [carry]) .
    foldl (\(outputs, carry) (x, y) -> let (z, carry') = halfAdder x y carry in (z : outputs, carry')) ([], Constant False)

ifThenElse c x y = (c `cAnd` x) `cXor` (Not c `cAnd` y)
ifThenElse' = map . uncurry . ifThenElse
ifThenElse2 c (x1, x2) (y1, y2) = (ifThenElse c x1 y1, ifThenElse c x2 y2)

cMaximum inputs = ifThenElse' (Not (ltUnsigned inputs)) inputs

inputList x n = [Input (x, i) | i <- [0..n]]
intInput = map Input . littleEndianDecomposition
intInputs a b = (pad (intInput a) (intInput b) (Constant False))

tests = do
    quickCheck $ \a b -> (a < b) == evalC id (ltBit (Input a) (Input b)) -- bit-less-than correct
    quickCheck $ \a b -> (a || b) == evalC id (cOr (Input a) (Input b)) -- bit-or correct
    quickCheck $ \x -> x >= (0 :: Integer) ==> littleEndianComposition (littleEndianDecomposition x) == x -- littleEndian{Dec,C}omposition inverses
    print $ ltUnsigned (zip (inputList 'x' 2) (inputList 'y' 2))
    quickCheck $ \a b -> (a > (0 :: Integer) && b > 0) ==> ltUnsignedDirect a b == (a < b) -- bitwise unsigned-less-than correct
    quickCheck $ \a b -> (a > (0 :: Integer) && b > 0) ==> evalC id (ltUnsigned (intInputs a b)) == (a < b) -- circuit unsigned-less-than correct
    quickCheck $ \a b -> (a > (0 :: Integer) && b > 0) ==> littleEndianComposition (map (evalC id) (cMaximum (intInputs a b))) == max a b -- circuit unsigned-maximum correct

    quickCheck $ \a b -> (map (evalC id) (adder (intInputs (a :: Word8) (b :: Word8)))) == (map (evalC id) (adder (intInputs b a))) -- adder commutative
    quickCheck $ \a b -> (a :: Word8) + b == littleEndianComposition (map (evalC id) (adder (intInputs a b))) -- adder correct

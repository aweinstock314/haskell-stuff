{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Foldable as F
import System.Random

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Empty
    deriving (Eq, Show, Ord)

instance Foldable BinaryTree where
    foldr f acc Empty = acc
    foldr f acc (Node x l r) = F.foldr f (f x (F.foldr f acc r)) l

treeInsert x Empty = Node x Empty Empty
treeInsert x (Node y l r) = case compare x y of
    LT -> Node y (treeInsert x l) r
    GT -> Node y l (treeInsert x r)
    EQ -> Node y l r

treeFind x Empty = Empty
treeFind x (Node y l r) = case compare x y of
    LT -> treeFind x l
    GT -> treeFind x r
    EQ -> Node y l r

treeRemove x Empty = Empty
treeRemove x (Node y l r) = case compare x y of
    LT -> Node y (treeRemove x l) r
    GT -> Node y l (treeRemove x r)
    EQ -> case (l, r) of
        (Empty, Empty) -> Empty
        (_, Empty) -> l
        (Empty, _) -> r
        (_, _) -> let lMax = F.maximum l in
            Node lMax (treeRemove lMax l) r

sampleTree rng = F.foldr treeInsert Empty (take 10 $ randomRs (0, 50) rng)

--shuffle rng = map snd . sortBy (comparing fst) . zip (randoms rng :: [Int])

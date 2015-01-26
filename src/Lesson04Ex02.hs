module Lesson04Ex02 where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)

getLeft (Node l _ _) = l
getRight (Node _ _ r) = r
getValue (Node _ v _) = v

data Side = LeftS | RightS | BothS deriving (Show)

height :: Tree a -> Integer
height e = height' e 0
    where height' Leaf d = d
          height' (Node l _ r) d = max (height' l (d+1)) (height' r (d+1))

isBalanced :: Tree a -> Side
isBalanced Leaf = BothS
isBalanced (Node l _ r)
    | w <= -1 = LeftS
    | w == 0 = BothS
    | otherwise = RightS
    where w = height l - height r

foldTree :: [a] -> Tree a
foldTree = foldl add Leaf

-- add insère un élement et retour un arbre équilibré
add :: Tree a -> a -> Tree a
add Leaf a = Node Leaf a Leaf
add n@(Node l a r) e = case s of
    RightS -> Node l a ( add r e)
    _ -> Node (add l e) a r
    where s = isBalanced n


main = do
    --testHeight (Node (Node Leaf 3 (Node Leaf 4 Leaf)) 1 (Node Leaf 2 Leaf))
    testIsBalanced Leaf
    testIsBalanced (Node Leaf 1 Leaf)
    testIsBalanced (Node (Node Leaf 2 Leaf) 1 Leaf)
    testIsBalanced (Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf))
    testFoldTree "ABCDEFGHIJ"
    where testHeight = print . height
          testIsBalanced x = putStrLn (show (height x)  ++ " " ++ show (isBalanced x))
          testFoldTree x = print (foldTree x)

module Lesson04Ex03 where

xor :: [Bool] -> Bool
xor = foldr1 xor' 

xor' True False = True
xor' False True = True
xor' True True = False
xor' False False = False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e ll -> f e : ll) []

foldl' f = foldr (\e ll -> ll ++ [f e])

main = do
    testXor [False, True, False]
    testXor [False, True, False, False, True]
    testMap (+1) [1..10]
    where testXor = print . xor
          testMap f = print . map f
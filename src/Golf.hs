module Golf where

import Data.List (group, sort)

skips :: [a] -> [[a]]
skips l = map (`extractEvery` l) [1..length l]

extractEvery :: Int -> [a] -> [a]
extractEvery n = map snd . filter fst . zip (cycle (replicate (n-1) False) ++ [True])

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (a:b:c:xs)
    | (a < b) && (c < b) = b : localMaxima (b:c:xs)
    | otherwise = localMaxima (b:c:xs)
localMaxima (_:xs) = localMaxima xs

histogram :: [Integer] -> String
histogram datas =  unlines (map (`generateLine` stats) (reverse [1..height stats]) ++ ["==========", "0123456789"])
    where stats = basicStats datas

generateLine line stats = concatMap (\col -> generateCell col line stats) [0..9]

generateCell col line stats = case x of
    [] -> " "
    _ -> "*"
    where x = filter (\(_,h) -> h >= line) $ filter (\(l,_) -> l == col) stats

basicStats :: (Ord a) => [a] -> [(a,Integer)]
basicStats l = map (\x -> (head x, toInteger (length x))) $ (group . sort) l

height :: [(a,Integer)] -> Integer
height = maximum . map snd

main = do
    {-testSkips "ABCD"
    testSkips "hello !"
    testSkips [1]
    testSkips [True, False]-}
    {-testLocalMaxima [2,9,5,6,1]
    testLocalMaxima [2,3,4,1,5]
    testLocalMaxima [1,2,3,4,5]-}
    testHistogram [1,1,1,5]
    testHistogram  [1,4,5,4,6,6,3,4,2,4,9]
    where testSkips x = print (skips x)
          testLocalMaxima x = print (localMaxima x)
          testHistogram = putStrLn . histogram

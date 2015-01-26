module Lesson04Ex04 where


couples :: Integer -> [(Integer,Integer)]
couples n = [(i,j) | i<- [1..n], j <- [1..n]]

--exclusions :: (Ord a, Num a, Enum a) => a -> [a]
exclusions n = filter (<=n) $ map (\(i,j) -> i+j+(2 * i * j)) (couples n)

--solve :: (Ord b, Num b, Enum b) => b -> [b]
solve n = map (\x -> 1 + x*2) $ filter (`notElem` ex) [1..n]
    where ex = exclusions n

main = print . solve $ 200

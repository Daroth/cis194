module Lesson04Ex01 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x-2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' l = sum . takeWhile (/= 1) $ iterate f l
    where f n | even n = n `div` 2
              | otherwise = 3 * n + 1

main = do
    testFun1 [1,2,3,4,5]
    testFun1 [1,5,9,15,3,10]
    mapM testFun2 [1..10]
    where testFun1 = testFunEq fun1 fun1'
          testFun2 = testFunEq fun2 fun2'
          testFunEq f1 f2 a = let x = show $ f1 a == f2 a in putStrLn (show a++" -> " ++ x ++ " " ++ show (f1 a) ++ " " ++ show (f2 a))

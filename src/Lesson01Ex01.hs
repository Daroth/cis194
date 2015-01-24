module Lesson01Ex01 where

import           Data.Char (digitToInt)

integerToIntegerList x = map (toInteger . digitToInt) (show x)

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | otherwise = integerToIntegerList x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther card = reverse (step1' (reverse card) 0)
    where step1' [] _ = []
          step1' (x:xs) 0 = x : (step1' xs 1)
          step1' (x:xs) 1 = (2 * x) : (step1' xs 0)

--step2 :: Num a => [a] -> Int
sumDigits :: [Integer] -> Integer
sumDigits card = sum (concatMap toDigits card)

validate :: Integer -> Bool
validate x = ((ttl x) `mod` 10) == 0
    where ttl  = fromInteger  . sumDigits  .doubleEveryOther .  integerToIntegerList

main = do
    putStrLn "Ex1"
    putStrLn . show $ validate 4012888888881881
    putStrLn . show $ validate 4012888888881882
    --putStrLn . show $ s2
    --putStrLn . show $ step3 s2
    --where s1 = doubleEveryOther  [1,3,8,6]
--          s2 = sumDigits s1

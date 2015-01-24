module Lesson01Ex01 where

import           Data.Char (digitToInt)

integerToIntegerList x = map (toInteger . digitToInt) (show x)

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | otherwise = integerToIntegerList x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] ->  [Integer]
doubleEveryOther xs = zipWith (*) (reverse $ take (length xs) $ cycle [1,2]) xs

sumDigits :: [Integer] -> Integer
sumDigits card = sum (concatMap toDigits card)

validate :: Integer -> Bool
validate x = ((ttl x) `mod` 10) == 0
    where ttl  = fromInteger  . sumDigits  .doubleEveryOther .  integerToIntegerList

main = do
    putStrLn "Ex1"
    putStrLn . show $ validate 4012888888881881
    putStrLn . show $ validate 4012888888881882
    putStrLn . show $ doubleEveryOther [8,7,6,5]
    putStrLn . show $ doubleEveryOther [1,2,3]

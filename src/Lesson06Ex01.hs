module Lesson06Ex01 where


--fib :: Int -> Int
fib x | x <= 2 = 1
      | otherwise = fib (x-1) + fib (x-2)

fibs1 :: Int -> [Int]
fibs1 x = take x $ map fib1 [0..]
    where fib1 0 = 1
          fib1 1 = 1
          fib1 n = fib1 (n-1) + fib1 (n-1)

--fibs2 :: Int -> [Int] 
--fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Stream a (Stream a)

streamtolist :: Stream a -> [a]
streamtolist (Stream a s) = a : streamtolist s

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a b) = Stream (f a) (streamMap f b)

streamFromSeed :: (a -> a ) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a))

streamFromSeed' :: (a -> a -> a) -> a -> a -> Stream a
streamFromSeed' f a1 a2 = Stream a1 (streamFromSeed' f a2 (f a1 a2))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = undefined

instance Show a => Show (Stream a) where
    show = show . take 10 . streamtolist

main = do
    putStrLn (show nats)
    putStrLn (show $ streamFromSeed' (+) 1 1)
    --testFibss 10
    --print (take . fibs1 10)
    ---print (fibs2 10)
    where
        --testFibss a = print (fibs1 a  == fibs2 a)
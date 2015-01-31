{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Course07 where

newtype Sum a = Sum a deriving (Ord, Eq, Show, Num)

getSum :: Sum a -> a
getSum (Sum a) = a

main = do
    print (Sum 1 + Sum 2)
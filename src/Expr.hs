module Expr where

class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a
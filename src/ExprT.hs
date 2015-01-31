module ExprT where

import           Expr

data ExprT = Lit Integer | Add ExprT ExprT | Mul ExprT ExprT deriving (Show, Eq)

instance  Expr ExprT where
    mul = Mul
    add = Add
    lit = Lit

instance Expr Int where
    mul = (*)
    add = (+)
    lit = \_ -> 1

instance Expr Bool where
    mul = (&&)
    add = (||)
    lit = (>0)

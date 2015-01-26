module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr a = do
    x <- parseExp Lit Add Mul a
    return (eval x)

main = do
    testEval (Lit 1)
    testEval (Add (Lit 1) (Lit 2))
    testEval (Mul (Add (Lit 1) (Lit 2)) (Lit 3))
    testEvalStr "1*2"
    testEvalStr "lol"
    where testEval = print . eval
          testEvalStr = print . evalStr
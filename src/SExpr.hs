{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

wrapped :: Parser a -> Parser b -> Parser b
wrapped p1 p2 = p1 *> p2 <* p1

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

spacesW :: Parser b -> Parser b
spacesW = wrapped spaces

parenthesis :: Parser b -> Parser b
parenthesis p = char '(' *> p <* char ')'

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


atom :: Parser Atom
atom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spacesW (A <$> atom <|> Comb <$> parenthesis (oneOrMore parseSExpr))

main = do
    print (runParser parseSExpr "5")
    print (runParser parseSExpr "foo3")
    print (runParser parseSExpr "(1)")
    print (runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)")
    print (runParser spaces " a")
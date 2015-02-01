{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Char
import JoinList

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

getScore (Score i) = i

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

onePointLetters = "eaionrtlsu"
twoPointsLetters = "dg"
threePointsLetters = "bcmp"
fourPointsLetters = "fhvwy"
fivePointsLetters = "k"
sixPointsLetters = "jx"
sevenPointsLetters = "qz"

score :: Char -> Score
score x | toLower x `elem` onePointLetters = 1
score x | toLower x `elem` twoPointsLetters = 2
score x | toLower x `elem` threePointsLetters = 3
score x | toLower x `elem` fourPointsLetters = 4
score x | toLower x `elem` fivePointsLetters = 5
score x | toLower x `elem` sixPointsLetters = 6
score x | toLower x `elem` sevenPointsLetters = 7
score _ = 0

scoreString :: String -> Score
scoreString = mconcat . map score

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x
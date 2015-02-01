{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (replicateM)
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

dices :: Army -> Int -> Int -> Rand StdGen [DieValue]
dices mx mn army = do
    dices <- replicateM (max mx (army - mn)) die
    return (reverse . sort $  dices)


solveBF :: Battlefield -> (DieValue, DieValue) -> Battlefield
solveBF (Battlefield ba bd) (a,d) | a > d = Battlefield ba (bd-1)
                                  | otherwise = Battlefield (ba -1) bd

solveBattleField :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
solveBattleField b a d = foldl solveBF b (zip a d)

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) = do
    dicesA <- dices 3 1 a
    dicesD <- dices 2 0 d
    return (solveBattleField b dicesA dicesD)

main = do
    result <- evalRandIO $ battle (Battlefield 14 2)
    print result
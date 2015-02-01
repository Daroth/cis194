{-# LANGUAGE FlexibleInstances #-}
module JoinListBuffer where


import Buffer
import JoinList
import Scrabble
import Sized

instance Buffer (JoinList (Score, Size) String) where
    toString = show
    fromString = undefined
    line = indexJ
    replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b
    numLines = sz
    value = getScore . fst . tag
  
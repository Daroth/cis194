module LogAnalysis where

import           Control.Applicative ((<$>))
import           Log

parseMessage :: String -> LogMessage
parseMessage line = case words line of
    ("I":timestamp:message) -> case reads timestamp of
        [] -> Unknown line
        [(tt,_)] -> LogMessage Info tt (unwords message)
    ("W":timestamp:message) -> case reads timestamp of
        [] -> Unknown line
        [(tt,_)] -> LogMessage Warning tt (unwords message)
    ("E":level:timestamp:message) -> case reads level of
        [] -> Unknown line
        [(lvl,_)] -> case reads timestamp of
            [] -> Unknown line
            [(tt,_)] -> LogMessage (Error lvl) tt (unwords message)
    _ -> Unknown line

parseAll  = map parseMessage . lines

organizeAll :: [LogMessage]  -> MessageTree
organizeAll = foldl organizeOne Leaf

organizeOne :: MessageTree -> LogMessage -> MessageTree
organizeOne x (Unknown _)  = x
organizeOne Leaf x = Node Leaf x Leaf
organizeOne (Node bl lm1@(LogMessage _ tt1 _) br) lm2@(LogMessage _ tt2 _)
     | tt2 > tt1 = case br of
         Leaf -> Node bl lm1 (Node Leaf lm2 Leaf)
         _ ->  Node bl lm1 (organizeOne br lm2)
     | otherwise = case bl of
         Leaf -> Node (Node Leaf lm2 Leaf) lm1 br
         _ -> Node (organizeOne bl lm2) lm1 br

getSorted :: MessageTree -> [LogMessage]
getSorted Leaf = []
getSorted (Node bl lm br) = getSorted bl ++ [lm] ++ getSorted br

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = map getMessage (filter criteria l)
    where criteria (LogMessage (Error lvl) _ _ ) = lvl >= 50
          criteria _ = False

getMessage (Unknown m ) = m
getMessage (LogMessage _ _ m) = m


main = do
    x <- parseAll <$> readFile "error.log"
    print . whatWentWrong . getSorted . organizeAll $ x

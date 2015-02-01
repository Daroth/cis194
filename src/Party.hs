module Party where

import Employee
import Data.Monoid
import Data.Foldable as F
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (f + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0
    (GL l f) `mappend` (GL l' f') = GL (l ++ l') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl@(GL _ f) gl'@(GL _ f')
    | f >= f' = gl
    | otherwise = gl'

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold = F.foldl


nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel = undefined

process :: Tree Employee -> GuestList
process _ = GL [(Emp "A" 1), (Emp "B" 2)] 10

format :: GuestList -> String
format (GL l _) = unlines . (fmap empName) $ l


main = do
    content <- readFile "employee.txt"
    putStrLn . format . process $ read content
    -- read
    -- process
    -- displayResult



module JoinList where

import           Data.Monoid
import           Sized

data JoinList m a = Empty | Single m a | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


sz :: (Sized b, Monoid b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ n jl | n > sz jl = Nothing
indexJ n (Single _ v) = Just v
indexJ n (Append m ljl rjl)
  | n < sz ljl = indexJ n ljl
  | otherwise = indexJ (n - sz ljl) rjl


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a ->JoinList b a
dropJ _ Empty        = Empty
dropJ n jl | n <= 0  = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append m left right)
  | n < sz left = (dropJ n left) +++ right
  | otherwise   = dropJ (n - sz left) right


takeJ :: (Monoid b, Sized b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl | n > sz jl = jl
takeJ n jl | n <= 0 = Empty
takeJ n s@(Single _ _) = s
takeJ n (Append m ljl rjl)
  | n < sz ljl = takeJ n ljl
  | otherwise  = ljl +++ (takeJ (n - sz ljl) rjl)

  
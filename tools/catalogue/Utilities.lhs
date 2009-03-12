> module Utilities where

> split       :: Eq a => a -> [a] -> [[a]]
> split delim = foldr f [[]]
>   where
>     f x rest@(r:rs)
>       | x == delim = [] : rest
>       | otherwise  = (x : r) : rs

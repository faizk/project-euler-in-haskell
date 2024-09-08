module Lib
    ( int
    ) where

-- interestion of ordered lists.
int :: Ord a => [a] -> [a] -> [a]
int (x:xs) (y:ys) | x< y  = int xs (y:ys)
int (x:xs) (y:ys) | x > y  = int (x:xs) ys
int (x:xs) (_:ys)          = x : int xs ys
int [] _                   = []
int _ []                   = []

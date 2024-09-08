module Lib
    ( int
    , isPrime
    ) where

-- interestion of ordered lists.
int :: Ord a => [a] -> [a] -> [a]
int (x:xs) (y:ys) | x< y  = int xs (y:ys)
int (x:xs) (y:ys) | x > y  = int (x:xs) ys
int (x:xs) (_:ys)          = x : int xs ys
int [] _                   = []
int _ []                   = []

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
isPrime n = all ((/= 0) . (n `mod`)) [2 .. floor (sqrt (fromIntegral n))]

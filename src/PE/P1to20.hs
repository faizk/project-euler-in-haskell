module PE.P1to20
    ( multiplesOf3Or5, sumOfMultiplesOf3Or5Below1000
    ) where

multiplesOf3Or5 :: Int -> [Int]
multiplesOf3Or5 below =
  [x | x <- [1 .. below-1], x `multOf` 3 || x `multOf` 5]
  where
    multOf n d = n `rem` d == 0

sumOfMultiplesOf3Or5Below1000 :: Int
sumOfMultiplesOf3Or5Below1000 = sum $ multiplesOf3Or5 1000

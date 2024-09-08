{-# LANGUAGE NumericUnderscores #-}
module PE.P1to20
    ( multiplesOf3Or5, sumOfMultiplesOf3Or5Below1000
    , fibonacci, sumOfEvenFibonacciTermsWithin4MM
    , primes, primeFactors, primeFactors'
    , largestPalindrimeProduct
    , smallestMultiple
    , suSqDiff, suSqDiffNaive
    ) where

import Data.List (find)

import Numeric.Natural

multiplesOf3Or5 :: Int -> [Int]
multiplesOf3Or5 below =
  [x | x <- [1 .. below-1], x `multOf` 3 || x `multOf` 5]
  where
    multOf n d = n `rem` d == 0

sumOfMultiplesOf3Or5Below1000 :: Int
sumOfMultiplesOf3Or5Below1000 = sum $ multiplesOf3Or5 1000

-- P2
fibonacci :: [Integer]
fibonacci = 1 : fib' 1 2 where fib' a b = b : fib' b (a+b)

sumOfEvenFibonacciTermsWithin4MM :: Integer
sumOfEvenFibonacciTermsWithin4MM =
  sum $ takeWhile (<= limit) $ filter even fibonacci
    where limit = 4_000_000

-- P3
primes :: [Integer]
primes = sieve [2..]
  where
    -- sieve (n:ns) = n : sieve (filter (not . (n `factorOf`)) ns)
    sieve (n:ns) = n : sieve [x | x <- ns, x `rem` n /= 0]
    sieve [] = []

factorOf :: Integer -> Integer -> Bool
factorOf d n = n `rem` d == 0

{- naive version -}
primeFactors' :: Integer -> [Integer]
primeFactors' n = filter (`factorOf` n) $ takeWhile (<= n) primes

primeFactors :: Integer -> [Integer]
primeFactors n =
  --sweep (takeWhile (<= root n) primes) n
  sweep (takeWhile (<= root n) (2 : filter odd [3..])) n
  where
    root n = floor $ sqrt $ fromIntegral n
    sweep (p:ps) n' | n' `rem` p /= 0 = sweep ps n'
    sweep (p:ps) n' = p : sweep (takeWhile (<= root n'') ps) n''
      where n'' = n' `red` p
    sweep _ n' | n' <= 1 = []
    sweep [] n' = [n']
    red n' d = case n' `divMod` d of
      (n'', 0) -> red n'' d
      (_, _) -> n'

-- P4
largestPalindrimeProduct :: Natural -> Integer
largestPalindrimeProduct digits =
  foldr max 0 $ filter ((\s -> reverse s == s) . show) ns
    where ns   = [x*y | x <- ddd, y <- ddd']
          ddd  = [s..e]
          ddd' = filter (\n -> n `rem` 11 == 0) ddd
          s = floor $ 10**(fromIntegral digits - 1)
          e = floor $ (10**fromIntegral digits) -1

-- P5
{- 2520 is the smallest number that can be divided by each of the
   numbers from to 1 to 10 without any remainder.
   What is the smallest positive number that is evenly divisible
   by all of the numbers from 1 to 20 ?
-}
smallestMultiple :: Integer -> Integer -> Maybe Integer
smallestMultiple from to = find f $ map (*jump) [1..]
  where f n = all (($ n) . divisibleBy) [from..to]
        divisibleBy d n = n `rem` d == 0
        jump = product $ takeWhile (<= to) primes

-- P6
suSqDiff :: [Integer] -> Integer
suSqDiff ns = sum [ a*b | (i,a) <- ind ns, (j,b) <- ind ns, i /= j ]
  where ind = zip [1..]

suSqDiffNaive :: [Integer] -> Integer
suSqDiffNaive ns = sqsu - susq
  where sqsu = let s = sum ns in s * s
        susq = sum $ map (\i -> i*i) ns

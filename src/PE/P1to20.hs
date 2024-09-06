{-# LANGUAGE NumericUnderscores #-}
module PE.P1to20
    ( multiplesOf3Or5, sumOfMultiplesOf3Or5Below1000
    , fibonacci, sumOfEvenFibonacciTermsWithin4MM
    , primes, primeFactors
    ) where

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
primes = sieve [] [2..]
  where
    sieve ps (n:ns) = n : sieve (n:ps) (filter (not . (n `factorOf`)) ns)
    sieve ps [] = ps

factorOf :: Integer -> Integer -> Bool
factorOf d n = n `rem` d == 0

primeFactors :: Integer -> [Integer]
primeFactors n = filter (`factorOf` n) $ takeWhile (<= n) primes

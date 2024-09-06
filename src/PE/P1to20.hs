{-# LANGUAGE NumericUnderscores #-}
module PE.P1to20
    ( multiplesOf3Or5, sumOfMultiplesOf3Or5Below1000
    , fibonacci, sumOfEvenFibonacciTermsWithin4MM
    , primes, primeFactors, primeFactors'
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
  sweep (takeWhile (<= n) primes) n
  where
    sweep (p:ps) n' | n' `rem` p /= 0 = sweep ps n'
    sweep (p:ps) n' = p : sweep (takeWhile (<= n'') ps) n''
      where n'' = n' `red` p
    sweep _ n' | n' <= 1 = []
    sweep [] n' = [n']
    red n' d = case n' `divMod` d of
      (n'', 0) -> red n'' d
      (_, _) -> n'

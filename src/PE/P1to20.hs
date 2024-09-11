{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
module PE.P1to20
    ( multiplesOf3Or5, sumOfMultiplesOf3Or5Below1000
    , fibonacci, sumOfEvenFibonacciTermsWithin4MM
    , primes, primeFactors, primeFactors'
    , largestPalindrimeProduct
    , smallestMultiple
    , suSqDiff, suSqDiffNaive
    , largestProductInSeries, thousandDigits
    , pythagoreanTriplets, p9
    , primes', p10SumOfPrimes
    , p11Input, fetchLine, draw, largestProductInGrid, p11Ans
    ) where

import Data.List (find)

import Numeric.Natural
import Text.RawString.QQ
import Data.Char (digitToInt, isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Data.Vector ((!?))
import Data.Semigroup (Max(Max))


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

-- P8
-- Largest Product in a Series
largestProductInSeries :: Int -> [Natural] -> Natural
largestProductInSeries win = maximum . map product . window'
  where window []      = []
        window (x:xs)  = take win (x:xs) : window xs
        window'        = filter ((== win) . length) . filter (all (>= 1)) . window

thousandDigits :: [Natural]
thousandDigits = map (fromIntegral . digitToInt) $
  filter isDigit thousandDigitString

thousandDigitString :: String
thousandDigitString = [r|
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
|]

-- P9
-- Special Pythagorean Triplet
pythagoreanTriplets :: [(Integer, Integer, Integer)]
pythagoreanTriplets =
  [(a, b, c) | c<-[1..], b<-[1..(c-1)], a<-[1..(b-1)], a*a + b*b == c*c]

p9 ::  [((Integer, Integer, Integer), Integer)]
p9 = [((a,b,c), a*b*c) | (a,b,c) <- pythagoreanTriplets, a+b+c==1000]

-- P10
-- Summation of Primes

primes' :: [Integer]
primes' = filter isPrime cands
  where
    isPrime n = all ((/= 0) . (n `mod`)) $ takeWhile (<= lim) cands
      where lim = floor $ sqrt (fromIntegral n :: Double)
    cands = 2:3:5:7: [ i | i<-[11..], not (any (i `divBy`) [2,3,5,7])]
    divBy n d = n `rem` d == 0

p10SumOfPrimes :: Integer -> Integer
p10SumOfPrimes lim = sum $ takeWhile (<= lim) primes'

-- P11
-- Largest Product in a Grid

nxt :: (Int, Int) -> Int -> [[(Int, Int)]]
nxt (x,y) n          = ($ n) <$> [right, down, diag]
  where right = draw' ((+1), id)
        down  = draw' (id, (+1))
        diag  = draw' ((+1), (+1))
        draw' = draw (x,y)

type Grid a = V.Vector (V.Vector a)
type GridDir = (Int -> Int, Int -> Int)

draw :: (Int,Int) -> GridDir -> Int -> [(Int, Int)]
draw _     _        n | n <= 0 = []
draw (x,y) _        1          = [(x,y)]
draw (x,y) (xf, yf) n          = (x,y) : draw (xf x, yf y) (xf, yf) (n-1)

fetchLine :: (Int, Int) -> GridDir -> Grid a -> Int -> Maybe [a]
fetchLine (x,y) dir grid len = mapM fetch pts
  where pts = draw (x,y) dir len
        fetch (x', y') = grid !? x' >>= (!? y')

largestProductInGrid :: Grid Integer-> Int -> Integer
largestProductInGrid grid len =
  if rows + cols >= 1 then largest else 0
    where
      largest = maximum $ concatMap prods pts
      prods pt = mapMaybe (($ pt) . prod) [down, right, diag, diag']
      prod dir pt = product <$> fetchLine pt dir grid len

      down = ((+1), id)
      right = (id, (+1))
      diag = ((+1), (+1))
      diag' = ((+1), \x -> x - 1)
      pts = [(row,col)|row<-[0..rows-1], col<-[0..cols-1]]
      rows = length grid
      cols = foldr (max . length) 0 grid

p11Input :: Grid Integer
p11Input = V.fromList [V.fromList cols | cols <- lists]
  where
    lists = [
      [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
      [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
      [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
      [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
      [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
      [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
      [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
      [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
      [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
      [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
      [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
      [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
      [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
      [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
      [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
      [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
      [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
      [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
      [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
      [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]

p11Ans :: Integer
p11Ans = largestProductInGrid p11Input 4

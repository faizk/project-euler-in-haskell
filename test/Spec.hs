import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import qualified Data.Vector as V

import Lib (isPrime)
import qualified PE.P1to20
import PE.P1to20 (largestProductInGrid, divisors)

main :: IO ()
main = hspec $ do
  describe "P(1) Multiples of 3 or 5 : multiplesOf3Or5" $ do

    it "sum under 10 is 23" $ do
      sum (PE.P1to20.multiplesOf3Or5 10) `shouldBe` 23

  describe "P(2) Largest prime factor" $
    let primeFactors = PE.P1to20.primeFactors
        primeFactorsNaive = PE.P1to20.primeFactors'
    in do
    describe "primes" $ do
      prop "must return only prime numbers" $
        \n -> all isPrime $ take n PE.P1to20.primes
    describe "primeFactors" $ do
      prop "must never be empty for inputs greater than 1" $
        \i -> i > 1 ==> primeFactors i `shouldNotBe` []
      prop "must return only itself if given a prime number" $
        \n -> isPrime n ==> primeFactors n `shouldBe` [n]
      it "should return 5,7,13,29 given 13195" $ do
        primeFactors 13195 `shouldBe` [5,7,13,29]
      prop "must be as correct as the naive version" $
        \n -> primeFactors n `shouldBe` primeFactorsNaive n

  describe "P(5) Smallest Multiple" $ do
    describe "smallestMultiple" $ do
      it "returns 2520 for all numbers 1..10" $ do
        PE.P1to20.smallestMultiple 1 10 `shouldBe` Just 2520

  describe "P(6) Sum Square Difference" $ do
    describe "suSqDiff" $ do
      prop "must match it's naive version" $
        \l -> PE.P1to20.suSqDiff l `shouldBe` PE.P1to20.suSqDiffNaive l

  describe "P(11) Largest Product in a Grid" $ do
    describe "fetchLine" $
      let grid = PE.P1to20.p11Input
          fetchLine = PE.P1to20.fetchLine
        in do
      it "returns the numbers at the given position" $ do
        let p = (6, 8)
            got = fetchLine p ((+1), (+1)) grid 4
        got `shouldBe` Just [26,63,78,14]
      it "return Nothing for a line crossing out of the grid" $ do
        let p = (18, 18)
            got = fetchLine p ((+1), (+1)) grid 4
        got `shouldBe` Nothing
    describe "largestProductInGrid" $
      let grid = V.fromList [ V.fromList cols | cols <- grid' ]
          grid' = [[1, 2, 4, 5],
                   [1, 1, 3, 3],
                   [2, 5, 3, 1],
                   [3, 1, 3, 6]]
          largest = PE.P1to20.largestProductInGrid grid
        in do
      it "should return the largest product" $ do
        largest 1 `shouldBe` 6
        largest 2 `shouldBe` 20
        largest 3 `shouldBe` 75

  describe "P(12) Highly Divisible Triangular Number" $
    let decomp = PE.P1to20.primeDecompose
        uniqC = PE.P1to20.uniqC
        divs = PE.P1to20.divisors
        ndivs = PE.P1to20.numDivisors
    in do
    describe "primeDecompose" $ do
      prop "decomposes any number into it's prime components" $
        \n -> n >= 1 ==> do let ps = decomp n
                            product ps `shouldBe` n
                            ps `shouldSatisfy` all isPrime
    describe "uniqC" $ do
      it "should count consequtive repeats" $ do
        uniqC "" `shouldBe` []
        uniqC "a" `shouldBe` [('a', 1)]
        uniqC "aba" `shouldBe` [('a',1), ('b',1), ('a',1)]
        uniqC "abba" `shouldBe` [('a',1), ('b',2), ('a',1)]
    describe "primeDecompose . uniqC" $ do
      prop "decomposes any number into it's prime components" $
        \n -> n >= 1 ==>
          do let pd = PE.P1to20.uniqC . decomp
                 got = product $ map (uncurry (^)) (pd n)
             got `shouldBe` n
    describe "numDivisors" $ do
      prop "should return the number of divisors of any number" $
        \n -> ndivs n `shouldBe` length (divs n)

  describe "P(12) Large Sum" $
    let base10 = PE.P1to20.base10digits
        pow10 n p = n * (10^p)
        toInt = sum . map (uncurry pow10)
        result = PE.P1to20.p13Result
      in do
    prop "base10digits" $
      \n p -> n >= 0 && p >= 0 ==>
        toInt (base10 (n,p)) `shouldBe` (n `pow10` p)
    prop "p13Result" $
      \ns -> let expected = sum ns'
                 input = map show ns'
                 ns' = map abs ns
             in toInt (result input) `shouldBe` expected


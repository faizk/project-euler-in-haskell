import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Lib (isPrime)
import qualified PE.P1to20

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


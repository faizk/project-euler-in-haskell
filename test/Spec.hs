import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import qualified PE.P1to20

main :: IO ()
main = hspec $ do
  describe "P(1) Multiples of 3 or 5 : multiplesOf3Or5" $ do

    it "sum under 10 is 23" $ do
      sum (PE.P1to20.multiplesOf3Or5 10) `shouldBe` 23

  describe "P(2) Largest prime factor" $ do
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
    where
      primeFactors = PE.P1to20.primeFactors

-- UTILS
isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
isPrime n = all ((/= 0) . (n `mod`)) [2 .. floor (sqrt (fromIntegral n))]

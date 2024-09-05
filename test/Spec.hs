import Test.Hspec
import Test.QuickCheck

import qualified PE.P1to20

main :: IO ()
main = hspec $ do
  describe "P(1) Multiples of 3 or 5 : multiplesOf3Or5" $ do

    it "sum under 10 is 23" $ do
      sum (PE.P1to20.multiplesOf3Or5 10) `shouldBe` 23


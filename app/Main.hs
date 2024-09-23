{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import Options.Applicative

import System.IO (hPutStrLn, stderr)
import qualified PE.P1to20

data Args = Args
  { problem   :: Int
  }

args :: Parser Args
args = Args
  <$> option auto
      ( long "problem"
      <> short 'p'
      <> help "the problem to run"
      <> metavar "PROBLEM NUMBER"
      )

solutions :: (MonadFail m) => [(Int, (String, m String))]
solutions = [1..] `zip`
  [ ("Find the sum of all the multiples of or below 1000",
      return $ show PE.P1to20.sumOfMultiplesOf3Or5Below1000)
  , ("By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.",
      return $ show PE.P1to20.sumOfEvenFibonacciTermsWithin4MM)
  , ("What is the largest prime factor of the number 600,851,475,143?",
      return $ show $ PE.P1to20.primeFactors 600_851_475_143)
  , ("Find the largest palindrome made from the product of two 3-digit numbers.",
      return $ show $ let digits = 3 in PE.P1to20.largestPalindrimeProduct digits)
  , ("What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20 ?",
     return $ show $ PE.P1to20.smallestMultiple 1 20)
  , ("Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.",
     return $ show $ PE.P1to20.suSqDiff [1..100])
  , ("What is the What is the 10,001 st prime number?",
     return $ show $ take 1 $ drop 10_000 PE.P1to20.primes)
  , ("Find the thirteen adjacent digits in the 1000-digit number " ++ show PE.P1to20.thousandDigits ++ " that have the greatest product. What is the value of this product?",
     return $ show $ PE.P1to20.largestProductInSeries 13 PE.P1to20.thousandDigits)
  , ("There exists exactly one Pythagorean triplet for which a+b+c=1000. Find the product .",
     return $ show $ take 1 PE.P1to20.p9)
  , ("Find the sum of all the primes below two million.",
     return $ show $ PE.P1to20.p10SumOfPrimes 2_000_000)
  , ("What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20x20 grid?",
      return $ show PE.P1to20.p11Ans )
  , ("What is the value of the first triangle number to have over five hundred divisors?",
      return $ show $ take 1 $ dropWhile ((<= 500) . snd) PE.P1to20.triangleNumNumDivisors)
  , ("Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.",
      return $ show PE.P1to20.p13Ans)
  , ("Which starting number, under one million, produces the longest (collatz) chain?",
      return $ show PE.P1to20.p14Ans)
  , ("How many 'latice grid' routes are there through a 20x20 grid?",
      return $ show $ PE.P1to20.numRoutes 20 20)
  ]

runSolution :: Args -> IO ()
runSolution Args { problem } =
  maybe todo showSoln (lookup problem solutions)
    where
      showSoln (statement, io) =
        hPutStrLn stderr ("PROBLEM " ++ show problem ++ ":\t" ++ statement)
        >> io >>= (putStrLn . ("ANSWER:\t\t" ++))
      todo = putStrLn $ "# TODO (problem " ++ show problem ++ " not solved yet)"

main :: IO ()
main = execParser opts >>= runSolution
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Run the solution (if it exists) for a given problem in Project Euler problem set (See: https://projecteuler.net/archives)"
     <> header "Project Euler Solutions" )

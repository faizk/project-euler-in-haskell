{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Options.Applicative

import qualified PE.P1to20
import System.IO (hPutStrLn, stderr)

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

solutions :: [(Int, (String, IO String))]
solutions = [1..] `zip`
  [ ("Find the sum of all the multiples of or below 1000",
      return $ show PE.P1to20.sumOfMultiplesOf3Or5Below1000)
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

module ParConc.Chapter02 where

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (threadDelay)
import Control.DeepSeq (deepseq)
import Control.Parallel.Strategies

testPar :: (Int, Int)
testPar = runEval $ do
  a <- rpar slowComputation
  b <- rpar slowComputation
  return (a, b)

slowComputation :: Int
slowComputation = slow `seq` 1
  where slow = unsafePerformIO $ do
          putStr "start"
          threadDelay $ 1000000 * 2
          putStr "end"

{-# LANGUAGE TemplateHaskell #-}
module TemplateHask where

import Language.Haskell.TH
import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)


bigBadMathProblem :: Int -> Int
-- bigBadMathProblem i = unsafePerformIO $ threadDelay 10000 >> return (read (show i ++ "0"))
bigBadMathProblem i = unsafePerformIO $ do
  a <- threadDelay 1000000
  a `seq` return (read (show i ++ "0"))

foo :: Int -> Int
foo i = unsafePerformIO $ do
  a <- threadDelay 1000000
  a `seq` return (read (show i ++ "0"))

intToPat :: Int -> Pat
intToPat = LitP . IntegerL . toInteger

precomputeInteger :: Int -> Exp
precomputeInteger = LitE . IntegerL . toInteger . bigBadMathProblem

precompute xs = do
  let name = mkName "lookupTable"
      patterns = map intToPat xs
      fnBodies = map precomputeInteger xs
      precomputedClauses =
        zipWith (\body pat -> Clause [pat] (NormalB body) []) fnBodies patterns
      x' = mkName "x"
      lastClause = [Clause [VarP x'] (NormalB appBody) []]
      appBody = AppE (VarE (mkName "bigBadMathProblem")) (VarE x')
      clauses = precomputedClauses ++ lastClause
  return [FunD name clauses]

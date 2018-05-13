module SchemeParser.Types where

import Data.Complex (Complex)
import Data.Ratio (Rational)

import qualified Data.Map as M

import Text.Parsec

type LispParser = Parsec String (M.Map String LispVal)

data LispVal = LAtom String
             | LString String
             | LBool Bool
             | LChar Char
             | LNumber Integer
             | LFloat Float
             | LRatio Rational
             | LComplex (Complex Double)
             | LList [LispVal]
             | LDottedList [LispVal] LispVal
             | LVector [LispVal]
             deriving Eq

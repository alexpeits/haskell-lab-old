{-# LANGUAGE RecordWildCards #-}
module SchemeParser.Printer where

import Data.Complex (Complex(..))
import Data.Ratio (numerator, denominator)

import SchemeParser.Types
import SchemeParser.Error

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- lispval

showVal :: LispVal -> String
showVal (LString s) = "\"" ++ s ++ "\""
showVal (LChar c) = "#\\" ++ [c]
showVal (LAtom a) = a
showVal LNil = "nil"
showVal (LNumber n) = show n
showVal (LFloat f) = show f
showVal (LRatio r) = show (numerator r) ++ "/" ++ show (denominator r)
showVal (LComplex (r :+ i)) = show r ++ "+" ++ show i ++ "i"
showVal (LBool True) = "#true"
showVal (LBool False) = "#false"
showVal (LList xs) = "(" ++ unwordsList xs ++ ")"
showVal (LDottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal (LVector xs) = "#(" ++ unwordsList xs ++ ")"
showVal (LPrimFunc _) = "<primitive>"
showVal LFunc{..} = "(lambda (" ++ unwords (map show params) ++ vargs ++ ") ...)"
  where vargs = case vararg of Nothing -> ""
                               Just args -> " . " ++ args

instance Show LispVal where
  show = showVal

-- errors

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = message

instance Show LispError where
  show = showError

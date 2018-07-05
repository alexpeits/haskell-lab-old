{-# LANGUAGE RecordWildCards #-}
module SchemeParser.Eval where

import Data.Maybe (isJust, isNothing)
import Data.List (find)

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M
import qualified Text.Parsec as P

import SchemeParser.Types
import SchemeParser.Parser (parseExpr)
import SchemeParser.Error
import SchemeParser.Printer
import SchemeParser.Environment

readExpr' :: P.Parsec String () a -> String -> Scheme a
readExpr' parser input = case P.parse parser "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

readExprList = readExpr' (P.endBy parseExpr P.spaces)

load :: String -> Scheme [LispVal]
load filename = liftIO (readFile filename) >>= readExprList

eval :: LispVal -> Scheme LispVal
eval v@(LString _) = return v
eval v@(LChar _) = return v
eval v@(LBool _) = return v
eval v@(LNumber _) = return v
eval v@(LFloat _) = return v
eval v@(LRatio _) = return v
eval v@(LComplex _) = return v
eval (LList [LAtom "quote", v]) = return v
-- eval (LList [LAtom "quasiquote", v]) = v
-- eval (LList [LAtom "unquote", v]) = v
-- eval (LList [LAtom "unquote-splice", v]) = v
eval (LList [LAtom "if", pred, conseq, alt]) = do result <- eval pred
                                                  case result of
                                                    LBool True  -> eval conseq
                                                    LBool False -> eval alt
                                                    notBool     -> throwError $ TypeMismatch "bool" notBool
eval (LList (LAtom "cond" : clause : clauses)) = evalCond clause clauses
  where evalCond :: LispVal -> [LispVal] -> Scheme LispVal
        evalCond (LList [LAtom "else", condRes]) _ = eval condRes
        evalCond (LList [condTest, condRes]) (clause':clauses') = do
          result <- eval condTest
          case result of
            LBool True  -> eval condRes
            LBool False -> evalCond clause' clauses'
            notBool     -> throwError $ TypeMismatch "bool" notBool
        evalCond (LList [condTest, condRes]) [] = do
          result <- eval condTest
          case result of
            LBool True  -> eval condRes
            LBool False -> throwError $ Default "Exhausted clauses in `cond`"
            notBool     -> throwError $ TypeMismatch "bool" notBool
eval (LList (LAtom "case" : key : clause : clauses)) = eval key >>= (\res -> evalCase res clause clauses)
  where evalCase :: LispVal -> LispVal -> [LispVal] -> Scheme LispVal
        evalCase _ (LList [LAtom "else", caseRes]) _ = eval caseRes
        evalCase k (LList [LList caseChoices, caseRes]) (clause':clauses') =
          if k `elem` caseChoices then eval caseRes else evalCase k clause' clauses'
        evalCase k (LList [LList caseChoices, caseRes]) [] =
          if k `elem` caseChoices then eval caseRes else throwError (Default "Exhausted clauses in `case`")

eval (LList (LAtom "get" : [LString arg])) = getVar GlobalEnv arg
eval (LList (LAtom "set" : LString name : [val])) = setVar GlobalEnv name val
eval (LList (LAtom "define" : LAtom name : [val])) = eval val >>= defineVar GlobalEnv name
eval (LList (LAtom "define" : LList (LAtom name : paramList) : bdy)) =
  defineVar GlobalEnv name (LFunc (map showVal paramList) Nothing M.empty bdy)
-- eval (LList (LAtom "lambda" : LList paramList : bdy)) =  return $ LFunc (map showVal paramList) Nothing bdy
eval (LList [LAtom "printenv"]) = do env <- asks (getEnv GlobalEnv); liftIO (print env); return (LBool True)
eval (LList [LAtom "load", LString filename]) = load filename >>= fmap last . mapM eval
-- eval (LList (LAtom func : args)) = mapM eval args >>= applyPrim func
eval (LList (LAtom fname : fargs)) = do
  f <- getVar GlobalEnv fname
  args <- mapM eval fargs
  applyF f args
eval (LAtom var) = getVar GlobalEnv var
-- eval (LList (func : args)) = do f <- eval func
                                -- argVals <- mapM eval args
                                -- applyF func argVals

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyPrim :: String -> [LispVal] -> Scheme LispVal
applyPrim func args = maybe notFunc ($ args) $ M.lookup func primitives
  where notFunc = throwError $ NotFunction "Unrecognized primitive function" func

applyF :: LispVal -> [LispVal] -> Scheme LispVal
applyF (LPrimFunc func) args = func args
applyF LFunc{..} args = do
  let num = toInteger . length
      remainingArgs = drop (length params) args
      evalBody = last <$> mapM eval body
      -- bindVarArgs arg = case arg of
                          -- Just argName -> bindVars [(argName, LList remainingArgs)]
                          -- Nothing      -> pure
  if num params /= num args && isNothing vararg
    then throwError $ NumArgs (num params) args
    else
    do envR <- bindVars LocalEnv $ zip params args
       local (const envR) evalBody

primitives :: M.Map String ([LispVal] -> Scheme LispVal)
primitives = M.fromList
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("symbol?", unaryOp symbolp)
  , ("bool?", unaryOp boolp)
  , ("string?", unaryOp stringp)
  , ("number?", unaryOp numberp)
  , ("list?", unaryOp listp)
  , ("vector?", unaryOp vectorp)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("print", lispPrint)
  ]

lispPrint :: [LispVal] -> Scheme LispVal
lispPrint [x] = liftIO (print x) >> return LNil

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Scheme LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ v@[_] = throwError $ NumArgs 2 v
numericBinop op args = mapM unNum args >>= return . LNumber . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> Scheme LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp _ v@(_:_:_) = throwError $ NumArgs 1 v

boolBinop :: (LispVal -> Scheme a) -> (a -> a -> Bool) -> [LispVal] -> Scheme LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker (head args)
                                     right <- unpacker (args !! 1)
                                     return $ LBool (left `op` right)

numBoolBinop = boolBinop unNum
strBoolBinop = boolBinop unStr
boolBoolBinop = boolBinop unBool

unNum :: LispVal -> Scheme Integer
unNum (LNumber n) = return n
unNum notNum = throwError $ TypeMismatch "number" notNum

unStr :: LispVal -> Scheme String
unStr (LString s) = return s
unStr notStr = throwError $ TypeMismatch "string" notStr

unBool :: LispVal -> Scheme Bool
unBool (LBool b) = return b
unBool notBool = throwError $ TypeMismatch "bool" notBool

symbolp, stringp, numberp, boolp, listp, vectorp :: LispVal -> LispVal

symbolp (LAtom _) = LBool True
symbolp _ = LBool False

stringp (LString _) = LBool True
stringp _ = LBool False

numberp (LNumber _) = LBool True
numberp (LFloat _) = LBool True
numberp (LRatio _) = LBool True
numberp (LComplex _) = LBool True
numberp _ = LBool False

boolp (LBool _) = LBool True
boolp _ = LBool False

listp (LList _) = LBool True
listp (LDottedList _ _) = LBool True
listp _ = LBool False

vectorp (LVector _) = LBool True
vectorp _ = LBool False

car :: [LispVal] -> Scheme LispVal
car [LList (x:xs)] = return x
car [LDottedList (x:xs) _] = return x
car [notList] = throwError $ TypeMismatch "list" notList
car badArg = throwError $ NumArgs 1 badArg

cdr :: [LispVal] -> Scheme LispVal
cdr [LList (x : xs)] = return $ LList xs
cdr [LDottedList [_] x] = return x
cdr [LDottedList (_ : xs) x] = return $ LDottedList xs x
cdr [notList] = throwError $ TypeMismatch "list" notList
cdr badArg = throwError $ NumArgs 1 badArg

cons :: [LispVal] -> Scheme LispVal
cons [x, LList xs] = return $ LList (x:xs)
cons [x, LDottedList xl xr] = return $ LDottedList (x:xl) xr
cons [x, y] = return $ LDottedList [x] y
cons badArg = throwError $ NumArgs 2 badArg

eqv :: [LispVal] -> Scheme LispVal
eqv [LBool arg1, LBool arg2]               = return $ LBool $ arg1 == arg2
eqv [LNumber arg1, LNumber arg2]           = return $ LBool $ arg1 == arg2
eqv [LString arg1, LString arg2]           = return $ LBool $ arg1 == arg2
eqv [LAtom arg1, LAtom arg2]               = return $ LBool $ arg1 == arg2
eqv [LDottedList xs x, LDottedList ys y]   = eqv [LList $ xs ++ [x], LList $ ys ++ [y]]
eqv [LList arg1, LList arg2]               = do
  let unBool (LBool b) = b
  e <- mapM (\(x, y) -> unBool <$> eqv [x, y]) (zip arg1 arg2)
  return $ LBool $ (length arg1 == length arg2) && and e
eqv [_, _]                                 = return $ LBool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

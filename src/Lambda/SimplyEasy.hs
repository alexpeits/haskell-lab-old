module Lambda.SimplyEasy where

data Name
  = Const String
  | Bound Int
  | Unquoted Int
  deriving (Show, Eq)

data ITerm
  = Ann CTerm Type
  | Var Int
  | Par Name
  | ITerm :@: CTerm
  deriving (Show, Eq)

data CTerm
  = Inf ITerm
  | Lam CTerm
  deriving (Show, Eq)

data Type
  = TPar Name
  | Fun Type Type
  deriving (Show, Eq)

data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NPar Name
  | NApp Neutral Value

vpar :: Name -> Value
vpar = VNeutral . NPar

type Env = [Value]

evalI :: ITerm -> Env -> Value
evalI (Ann e _)   d = evalC e d
evalI (Par x)     d = vpar x
evalI (Var i)     d = d !! i
evalI (e1 :@: e2) d = vapp (evalI e1 d) (evalC e2 d)

vapp :: Value -> Value -> Value
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

evalC :: CTerm -> Env -> Value
evalC (Inf i) d = evalI i d
evalC (Lam e) d = VLam (\x -> evalC e (x : d))

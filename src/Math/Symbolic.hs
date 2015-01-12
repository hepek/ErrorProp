module Math.Symbolic
        ( Expr(..)
        , simplify
        , diff
        , eval, partEval
        , var, variablesOf)
        where

import Prelude hiding (lookup)
import Data.Maybe
import Data.List -- todo: change to Map
import Data.Function

data Expr a = Atom a
            | Sum  (Expr a) (Expr a)
            | Prod (Expr a) (Expr a)
            | Exp  (Expr a) (Expr a)
            | Log  (Expr a)
            | Sin  (Expr a)
            | Cos  (Expr a)
            | Rec  (Expr a)
            | Neg  (Expr a)
            | E
            | Symbol String
            deriving(Eq)

instance (Num a) => Num (Expr a) where
  (+) = Sum
  a - b = Sum a (Neg b)
  (*) = Prod
  negate = Neg
  signum = undefined
  abs    = undefined
  fromInteger a = Atom (fromInteger a)

instance (Floating a) => Fractional (Expr a) where
  a / b = Prod a (Rec b)
  fromRational a = Atom (fromRational a)

instance (Floating a) => Floating (Expr a) where
  pi    = Atom pi
  exp   = Exp E
  sqrt  = flip Exp (1/2)
  log   = Log
  sin   = Sin
  tan a = Prod (Sin a) (Rec (Cos a))
  cos   = Cos
  (**)  = Exp
  logBase a b = Prod (Log a) (Rec (Log b))
  asin  = undefined
  atan  = undefined
  acos  = undefined
  sinh  = undefined
  tanh  = undefined
  cosh  = undefined
  asinh = undefined
  atanh = undefined
  acosh = undefined

instance (Show a) => Show (Expr a) where
  show (Atom a)   = show a
  show (Symbol a) = a
  show (Prod a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Sum  a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Neg a)    = '-' : show a
  show (Rec a)    = "1/" ++ show a
  show (Sin a)    = "sin(" ++ show a ++ ")"
  show (Cos a)    = "cos(" ++ show a ++ ")"
  show (Log a)    = "log(" ++show a++ ")"
  show (Exp a b)  = "(" ++ show a ++ "**" ++ show b ++ ")"
  show (E)        = "e"

var = Symbol

s :: (Fractional a, Floating a, Eq a) => Expr a -> Expr a
s (Sum (Atom a) (Atom b))  = Atom (a+b)
s (Prod (Atom a) (Atom b)) = Atom (a*b)
s (Neg (Atom a))    = Atom (-a)
s (Rec (Atom a))    = Atom (1/a)
s (Sin (Atom a))    = Atom (sin a)
s (Cos (Atom a))    = Atom (cos a)
s (Log (Atom a))    = Atom (log a)
s (Exp E (Atom b))  = Atom (exp b)
s (Exp (Atom a) (Atom b)) = Atom (a ** b)
s (Sum (Atom 0) a)  = s a
s (Sum a (Atom 0))  = s a
s (Prod (Atom 0) a) = Atom 0
s (Prod a (Atom 0)) = Atom 0
s (Prod (Atom 1) a) = s a
s (Prod a (Atom 1)) = s a
s (Prod (Atom a) (Prod (Atom b) c)) = Prod (Atom $ a*b) c
s (Sum  a b) | a == b    = 2 * s a
             | otherwise = s a + s b
s (Prod a b) = s a * s b
s a = a


d :: (Floating a) => String -> Expr a -> Expr a
d x (Atom a)   = 0
d x (Sum a b)  = d x a + d x b
d x (Prod a b) = (d x a * b) + (a * d x b)
d x (Rec a)    = - (d x a)/(a * a)
d x (Neg a)    = -(d x a)
d x (Log a)    = 1/a * (d x a)
d x (Sin a)    = (cos a)*(d x a)
d x (Cos a)    = -(sin a)*(d x a)
d x (Exp E b)  = (exp b)*(d x b)
d x (Exp (Symbol a) (Atom b))
               | a == x    = (Atom b) * ((Symbol a) ** (Atom (b-1)))
               | otherwise = 0
d x (Exp a b)  = (a ** b)*((d x a) * b/a + (d x b) * log a)
d x (Symbol y) | x == y    = 1
               | otherwise = 0


simplify a = f a
  where
    b = s a
    f a | b == a = a
        | otherwise  = simplify b

-- | Symbolic differentiator
diff (Symbol x) a = simplify $ d x a
diff _ _ = error "First argument must be a symbol"


-- | Performs partial evaluation
partEval :: (Floating a, Eq a, Show a) => [(Expr a, a)] -> Expr a -> Expr a
partEval _ (Atom a)     = Atom a
partEval env (Sum a b)  = s $ (partEval env a) + (partEval env b)
partEval env (Prod a b) = s $ (partEval env a) * (partEval env b)
partEval env (Neg a)    = s $ -(partEval env a)
partEval env (Rec a)    = s $ recip (partEval env a)
partEval env s@(Symbol _) = case lookup s env of
                              Just a -> Atom a
                              Nothing -> s   
partEval env (Exp E b)  = s $ exp (partEval env b)
partEval env (E)        = s $ exp 1
partEval env (Exp a b)  = s $ partEval env a ** partEval env b
partEval env (Sin a)    = s $ sin (partEval env a)
partEval env (Cos a)    = s $ cos (partEval env a)
partEval env (Log a)    = s $ log (partEval env a)

variablesOf :: (Expr a) -> [Expr a]
variablesOf expr =
    nubBy  ((==) `on` getSym) $
    sortBy (compare `on` getSym) $
    ex expr
  where
    ex (Symbol s) = [Symbol s]
    ex (Sum a b)  = (ex a) ++ (ex b)
    ex (Prod a b) = (ex a) ++ (ex b)
    ex (Exp a b)  = (ex a) ++ (ex b)
    ex (Log a)    = ex a
    ex (Sin a)    = ex a
    ex (Cos a)    = ex a
    ex (Rec a)    = ex a
    ex (Neg a)    = ex a
    ex (Atom _)   = []

    getSym (Symbol a) = a
    getSym _  = error "not a symbol"

-- | Evaluates an expression with an environment defined by mapping from
--   Symbol -> value
eval env expr  = extract $ simplify $ partEval env expr
  where
    extract (Atom a) = a
    extract other = error $ "unbound variables in final expr: "  ++ show other

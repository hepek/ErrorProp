{-# language ParallelListComp #-}
-- | Module 'ErrorProp' is used to calculate error propagation in
--   linear and non-linear systems
module Math.ErrorProp
       (Measurement
        , Fn
        , um, cm
        , lint, nlt, variables
        , var
        , x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
        , transform
        , diag, takeDiag, trans, (>.), (><)
        , simplify
       )
       where

import Control.Applicative
import Data.List
import Math.Symbolic
import Math.SimpleMx

type Fn = Expr Double

type Mx  = [[Double]]
type Vec = [Double]


data Transf = Lt Mx            -- ^ linear transformations
            | Nt [Fn] [[Fn]]   -- ^ non linear transformaton

-- | Measurementment represented by measured value and corresponding
--  covariance matrix
data Measurement = Measurement Vec Mx
              deriving (Eq)

instance (Show Transf) where
  show (Lt m)    = intercalate "\n" (map show m)
  show (Nt fs _) = intercalate "\n" $ zipWith (\a b -> a ++ " = " ++ b)
                                               (map show ys) 
                                               (map show fs)
    where
      ys = map (Symbol.('o':).show) [1..] :: [Fn]


instance (Show Measurement) where
  show (Measurement x mSigma)
    | mSigma == mD = showv x d
    | otherwise    = showm x mSigma
    where
       d  = takeDiag mSigma
       mD = diag d

       showv xs sigmas = "x\t\tvar\n" ++
             unlines [ show x ++ "\t\t" ++ show s
                      | x <- xs
                      | s <- sigmas]
       showm xs sigmas = "x\t\tCov\n" ++
             unlines [ show x ++ "\t\t" ++ (unwords . intersperse "\t" $
                        map show line)
                      | x <- xs
                      | line <- sigmas]


errorSize x s = error $  "Input length mismatch. length(x) = " ++ show (length x)
                      ++ ", but length(s) or one of its components isn't"

-- | Measurement smart constructor: constructs uncorrelated sample
um :: [Double]   -- ^ measurement
   -> [Double]   -- ^ variance
   -> Measurement
um x sigma | (length x /= length sigma) = errorSize x sigma
           | otherwise = Measurement x $ diag sigma

-- | Measurement smart constructor: constructs correlated sample
cm :: [Double]   -- ^ measurement
   -> [[Double]] -- ^ covariance matrix
   -> Measurement
cm x sigmas | any (\y -> length y /= length x) sigmas
              || (length sigmas /= length x) =
                 errorSize x sigmas
            | otherwise = Measurement x sigmas


-- | Smart constructor for linear transformation
lint :: [[Double]] -- ^ A matrix representing linear transformation
   -> Transf
lint = Lt

-- | predefined symbolic values to be used in defining expression
xs@[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] =
  map (Symbol.('x':).show) [1..11] :: [Fn]

-- | Smart constructor of nonlinear transformation
--   e.g. nt [x1*x1, x2, sin(x3)]
nlt :: [Fn]       -- ^ A list of functions, one for each output parameter
   -> Transf
nlt fs = Nt fs1 (jacobian fs1)
  where
     fs1 = map simplify fs

var :: String -> Fn
var = Symbol

-- | Calculates Jacobian matrix
jacobian :: [Fn] -> [[Fn]]
jacobian fs = transpose [map d fs | d <- ds]
  where
    ds = [ diff s | s <- xs']
    xs' = variablesOf fs

variables :: Transf -> [Fn]
variables (Lt _) = []
variables (Nt fs _) = variablesOf fs

-- | extracts unbound variables from a nonlinear transformation
variablesOf :: [Fn] -> [Fn]
variablesOf fs =
    (nubBy$ \a b -> cmpSymbol a b == EQ) .
    sortBy cmpSymbol .
    concatMap ex $ fs
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

    cmpSymbol (Symbol s1) (Symbol s2) = compare s1 s2
    cmpSymbol _ _ = error "Attempt to compare non-symbol."



-- | evaluate non-linear transformation at operation point
operatingPoint :: Transf -> [(Fn, Double)] -> (Vec, Mx)
operatingPoint   (Nt fs fs') env =
  (map (eval' env) fs,
   map (map (eval' env)) fs')

transform :: Transf -> Measurement -> Measurement
transform (Lt mA)      (Measurement x mS) = Measurement (mA >. x) (mA >< mS >< trans mA)
transform nlt@(Nt fs _) (Measurement x mS) = Measurement f (mL >< mS >< trans mL)
  where (f,mL) = operatingPoint nlt (zip (variablesOf fs) x)

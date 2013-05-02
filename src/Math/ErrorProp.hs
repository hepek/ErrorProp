{-# language ParallelListComp #-}
-- | Module 'ErrorProp' is used to calculate error propagation in
--   linear and non-linear systems
module Math.ErrorProp
       (Measurement
        , Fn
        , um, cm
        , lt, nt
        , variablesOf
        , x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
        , transform
        , diag, takeDiag, trans, (>.), (><)
       )
       where

--TODO:
-- Create LinearAlgebra module

import Data.List
import Data.List.Split
import Control.Applicative

import Math.Symbolic

type Fn = Expr Double

data Transf = Lt Mx            -- ^ linear transformations
            | Nt [Fn] [[Fn]]   -- ^ non linear transformaton
            deriving (Show)

-- | Measurementment represented by measured value and corresponding
--  covariance matrix
data Measurement = Measurement Vec Mx
              deriving (Eq)

type Mx  = [[Double]]
type Vec = [Double]

-- | Matrix vector multiplication
(>.) :: (Num a) => [[a]] -> [a] -> [a]
mA >. x  = map (muladd x) (mA)
   where
     muladd x y = sum (zipWith (*) x y)

-- | Matrix multiplication
(><) :: (Num a) => [[a]] -> [[a]] -> [[a]]
mA >< mB = transpose [ mA >. col | col <- transpose mB ]


takeDiag :: [[a]] -> [a]
takeDiag m = [ m !! (i-1) !! (i-1) | i <- [1 .. length m]]

diag :: (Num a) => [a] -> [[a]]
diag d = chunksOf n $ intercalate zeros $ transpose [d]
  where
    zeros = replicate n 0
    n     = length d

trans :: [[a]] -> [[a]]
trans  = transpose

errorSize x s = error $ "Input length mismatch. length(x) = " ++ show (length x)
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


-- | Smart constructor for linear transformation
lt :: [[Double]] -- ^ A matrix representing linear transformation
   -> Transf
lt = Lt

-- | predefined symbolic values to be used in defining expression
xs@[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] =
  map (Symbol.('x':).show) [1..11] :: [Fn]

ts@[t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11] =
  map (Symbol.('t':).show) [1..11] :: [Fn]

-- | Smart constructor of nonlinear transformation
--   e.g. nt [x1*x1, x2, sin(x3)]
nt :: [Fn]       -- ^ A list of functions, one for each output parameter
   -> Transf
nt fs = Nt fs (jacobian fs)
-- | Calculates Jacobian matrix

jacobian :: [Fn] -> [[Fn]]
jacobian fs = transpose [map d fs | d <- ds]
  where
    ds = [ diff s | s <- xs']
    xs' = variablesOf fs

-- | extracts variables from a list of expressions
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
operatingPoint :: Transf -> [Double] -> (Vec, Mx)
operatingPoint   (Nt fs fs') x =
  (map (eval' env) fs,
   map (map (eval' env)) fs')
  where
    env = zip xs x

transform :: Transf -> Measurement -> Measurement
transform (Lt mA)      (Measurement x mS) = Measurement (mA >. x) (mA >< mS >< trans mA)
transform nlt@(Nt _ _) (Measurement x mS) = Measurement f (mL >< mS >< trans mL)
     where (f,mL) = operatingPoint nlt x

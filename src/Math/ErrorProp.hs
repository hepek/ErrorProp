{-# language ParallelListComp #-}

-- | Module 'ErrorProp' is used to calculate error propagation in
--   linear and non-linear systems
module Math.ErrorProp
       (Measurement
        , um, cm
        , lt, nt
        , x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
        , t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11
        , linearT, nonLinearT
       )
       where

import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra
import Data.List

import Control.Applicative

import Math.Symbolic

type Mx  = Matrix Double
type Vec = Vector Double


-- | Measurementment represented by measured value and corresponding
--  covariance matrix
data Measurement = Measurement Vec Mx
              deriving (Eq)

errorSize x s = error "Input Length mismatch"

-- | Measurement smart constructor: constructs uncorrelated sample
um :: [Double] -> [Double] -> Measurement
um x sigma | (length x /= length sigma) = errorSize x sigma
           | otherwise = Measurement (fromList x) $ diag (fromList sigma)

-- | Measurement smart constructor: constructs correlated sample
cm :: [Double] -> [[Double]] -> Measurement
cm x sigmas | (or $ (map (\y -> length y /= length x) sigmas))
                  || ((length sigmas) /= (length x)) =
                 errorSize x sigmas
            | otherwise = Measurement (fromList x) (fromLists sigmas)


instance (Show Measurement) where
  show (Measurement x mSigma)
    | (mSigma == mD) = showv x d
    | otherwise      = showm x mSigma
    where
       d  = takeDiag mSigma
       mD = diag d

       showv x sigmas = "x\tσ\n" ++
             (unlines [ show x ++ "\t" ++ show s
                      | x <- toList x
                      | s <- toList sigmas])
       showm x sigmas = "x\tΣ\n" ++
             (unlines [ show x ++ "\t" ++ (unwords.(intersperse "\t") $
                        (map show line))
                      | x <- toList x
                      | line <- toLists sigmas])


-- | A type to represent linear transformation
type Lt = Mx

-- | Smart constructor for linear transformation
lt :: [[Double]] -> Lt
lt = fromLists

-- | performs linear transformation on the sample
linearT :: Lt -> Measurement -> Measurement
linearT mA (Measurement x mS) = Measurement (mA <> x) (mA <> mS <> trans mA)


-- | predefined symbolic values to be used in defining expression
xs@[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] = map (Symbol.('x':).show) [1..11]
ts@[t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11] = map (Symbol.('t':).show) [1..11]

type Fn = Expr Double

-- | Represents non-linear transformations
data Nt = Nt [Fn] [[Fn]]
  deriving (Show)

-- | Smart constructor of nonlinear transformation
--   e.g. nt [x1*x1, x2, sin(x3)]
nt :: [Fn] -> Nt
nt fs = Nt fs (jacobian fs)

-- | Calculates Jacobian matrix
jacobian :: [Fn] -> [[Fn]]
jacobian fs = transpose [map d fs | d <- ds]
  where
    ds = [diff s | s <- xs']
    xs' = takeWhile (\(Symbol x) -> x <= (maxDegreeX fs)) xs

maxDegreeX fs = l.sort.(map (l.sort.hx)) $ fs
  where
    l [] = "x1"
    l xs = last xs

hx x@(Symbol s) | x `elem` xs = [s]
                | otherwise   = []
hx (Sum a b)  = (hx a) ++ (hx b)
hx (Prod a b) = (hx a) ++ (hx b)
hx (Exp a b)  = (hx a) ++ (hx b)
hx (Log a)    = hx a
hx (Sin a)    = hx a
hx (Cos a)    = hx a
hx (Rec a)    = hx a
hx (Neg a)    = hx a
hx (Atom _) = []

-- | evaluate non-linear transformation at operation point
--   represented by vector t and a measurement
operatingPoint :: Nt -> [Double] -> [Double] -> (Vec, Mx)
operatingPoint (Nt fs fs') x t =
  (fromList  $ map (evalS env) fs,
   fromLists $ map (map (evalS env)) fs')
  where
    env = (toEnv xs x) ++ (toEnv ts t)
    toEnv symb val = zipWith (,) symb val

-- | Performs non-linear transformation
nonLinearT :: Nt -> [Double] -> Measurement -> Measurement
nonLinearT nlt t (Measurement x s) =
   Measurement f (mL <> s <> trans mL)
     where (f,mL) = operatingPoint nlt (toList x) t

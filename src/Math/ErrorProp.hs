{-# language ParallelListComp #-}

-- | Module 'ErrorProp' is used to calculate error propagation in
--   linear and non-linear systems
module Math.ErrorProp
       (Measurement
        , Fn
        , um, cm
        , lt, nt
        , x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
        , transform
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

type Fn = Expr Double

data Transf = Lt Mx            -- ^ linear transformations
            | Nt [Fn] [[Fn]]   -- ^ non linear transformaton
            deriving (Show)

-- | Measurementment represented by measured value and corresponding
--  covariance matrix
data Measurement = Measurement Vec Mx
              deriving (Eq)

errorSize x s = error $ "Input length mismatch. length(x) = " ++ show (length x)
                      ++ ", but length(s) or one of its components isn't"

-- | Measurement smart constructor: constructs uncorrelated sample
um :: [Double]   -- ^ measurement
   -> [Double]   -- ^ variance
   -> Measurement
um x sigma | (length x /= length sigma) = errorSize x sigma
           | otherwise = Measurement (fromList x) $ diag (fromList sigma)

-- | Measurement smart constructor: constructs correlated sample
cm :: [Double]   -- ^ measurement
   -> [[Double]] -- ^ covariance matrix
   -> Measurement
cm x sigmas | any (\y -> length y /= length x) sigmas
              || (length sigmas /= length x) =
                 errorSize x sigmas
            | otherwise = Measurement (fromList x) (fromLists sigmas)


instance (Show Measurement) where
  show (Measurement x mSigma)
    | mSigma == mD = showv x d
    | otherwise    = showm x mSigma
    where
       d  = takeDiag mSigma
       mD = diag d

       showv x sigmas = "x\t\tvar\n" ++
             unlines [ show x ++ "\t\t" ++ show s
                      | x <- toList x
                      | s <- toList sigmas]
       showm x sigmas = "x\t\tCov\n" ++
             unlines [ show x ++ "\t\t" ++ (unwords . intersperse "\t" $
                        map show line)
                      | x <- toList x
                      | line <- toLists sigmas]


-- | Smart constructor for linear transformation
lt :: [[Double]] -- ^ A matrix representing linear transformation
   -> Transf
lt = Lt . fromLists

-- | predefined symbolic values to be used in defining expression
xs@[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] =
  map (Symbol.('x':).show) [1..11] :: [Fn]

-- | Smart constructor of nonlinear transformation
--   e.g. nt [x1*x1, x2, sin(x3)]
nt :: [Fn]       -- ^ A list of functions, one for each output parameter
   -> Transf
nt fs = Nt fs (jacobian fs)

-- | Calculates Jacobian matrix
jacobian :: [Fn] -> [[Fn]]
jacobian fs = transpose [map d fs | d <- ds]
  where
    ds = [ simplify.(diff s) | s <- xs']
    xs' = takeWhile (\(Symbol x) -> x <= maxDegreeX fs) xs

maxDegreeX fs = l . sort . map (l.sort.hx) $ fs
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
operatingPoint :: Transf -> [Double] -> (Vec, Mx)
operatingPoint   (Nt fs fs') x =
  (fromList  $ map (evalS env) fs,
   fromLists $ map (map (evalS env)) fs')
  where
    env = zip xs x

transform :: Transf -> Measurement -> Measurement
transform (Lt mA)      (Measurement x mS) = Measurement (mA <> x) (mA <> mS <> trans mA)
transform nlt@(Nt _ _) (Measurement x mS) = Measurement f (mL <> mS <> trans mL)
     where (f,mL) = operatingPoint nlt (toList x)

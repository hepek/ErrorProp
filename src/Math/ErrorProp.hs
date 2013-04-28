{-# language ParallelListComp, CPP #-}
module Math.ErrorProp
       (Measure
        , mU, mC
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

-- | A type to represent linear transformation
type Lt = Mx

lt :: [[Double]] -> Lt
lt = fromLists

-- | Measurement or mean and sigma/covariance matrix
data Measure = Uncorr Vec Vec
             | Corr   Vec Mx
              deriving (Eq)

mU :: [Double] -> [Double] -> Measure
mU x sigma = Uncorr (fromList x) (fromList sigma)

mC :: [Double] -> [[Double]] -> Measure
mC x sigmas = Corr (fromList x) (fromLists sigmas)


instance (Show Measure) where
  show (Uncorr x sigmas) = "x\tσ\n" ++
    (unlines [ show x ++ "\t" ++ show s
                | x <- toList x
                | s <- toList sigmas])
  show (Corr x sigmas) = "x\tΣ\n" ++
    (unlines [ show x ++ "\t" ++ (unwords.(intersperse "\t") $ (map show line))
                | x <- toList x
                | line <- toLists sigmas])


normalize :: Measure -> Measure
normalize m@(Uncorr x s) = m
normalize m@(Corr x s) =
  case (s == mD) of
    True  -> Uncorr x d
    False -> m
  where
    d  = takeDiag s
    mD = diag d

linearT :: Lt -> Measure -> Measure
linearT mA (Uncorr x s) = normalize $ Corr (mA <> x) (mA <> diag s <> trans mA)
linearT mA (Corr  x mS) = normalize $ Corr (mA <> x) (mA <> mS <> trans mA)

xs@[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] = map (Symbol.('x':).show) [1..11]
ts@[t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11] = map (Symbol.('t':).show) [1..11]


data Nt = Nt [Expr Double] Lap
  deriving (Show)

data Lap = Lap [[Expr Double]]
           deriving (Show)

--laplacian :: [Expr Double] -> Lap
laplacian fs = Lap $ transpose [map d fs | d <- ds]
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

nt :: [Expr Double] -> Nt
nt fs = Nt fs (laplacian fs)

operatingPoint :: Nt -> [Double] -> [Double] -> (Vec, Mx)
operatingPoint (Nt fs (Lap fs')) x t =
  (fromList  $ map (evalS env) fs,
   fromLists $ map (map (evalS env)) fs')
  where
    env = (toEnv xs x) ++ (toEnv ts t)
    toEnv symb val = zipWith (,) symb val

nonLinearT :: Nt -> [Double] -> Measure -> Measure
nonLinearT nlt t (Uncorr x s) =
   normalize $ Corr f (mL <> diag s <> trans mL)
     where (f,mL) = operatingPoint nlt (toList x) t
nonLinearT nlt  t (Corr  x mS) =
   normalize $ Corr f (mL <> mS <> trans mL)
     where (f,mL) = operatingPoint nlt (toList x) t

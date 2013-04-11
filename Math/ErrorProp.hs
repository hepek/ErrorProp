{-# language ParallelListComp #-}
module ErrorProp
       where

import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra
import Data.List

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
    d = takeDiag s
    mD = diag d

linearT :: Lt -> Measure -> Measure
linearT mA (Uncorr x s) = normalize $ Corr (mA <> x) (mA <> diag s <> trans mA)
linearT mA (Corr  x mS) = normalize $ Corr (mA <> x) (mA <> mS <> trans mA)

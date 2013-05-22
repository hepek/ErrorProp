{-# language ParallelListComp #-}
-- | Module 'ErrorProp' is used to calculate error propagation in
--   non-linear systems
module Math.ErrorProp
       (Measurement
        , Fn, Transf
        , mkTransf
        , (+-), measurement, measurementCov
        , getCovariance, getValues
        , variables
        , defVar
        , x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
        , apply
        , linError
        , partial
        , simplify
       )
       where

import Control.Applicative
import Data.List
import Data.Function

import Math.Symbolic
import Math.SimpleMx

type Fn = Expr Double

data Transf = Nt [Fn] [[Fn]]   -- ^ non linear transformaton

-- | Measurementment represented by measured value(s) and corresponding
--  covariance matrix
data Measurement = Measurement (Vec Double) (Mx Double)
              deriving (Eq)

instance (Show Transf) where
  show (Nt fs _) = intercalate "\n" $ zipWith (\a b -> a ++ " = " ++ b)
                                               (map show ys) 
                                               (map show fs)
    where
      ys = map (Symbol.('o':).show) [1..] :: [Fn]


instance (Show Measurement) where
  show (Measurement x mSigma)
    | mSigma == mD = showv (toList x) d
    | otherwise    = showm (toList x) mSigma
    where
       d  = takeDiag mSigma
       mD = diag d

       showv xs sigmas = "measurement [\n" ++
         intercalate ",\n" 
           [ "  " ++ show x ++ " +- " ++ show s
               | x <- xs
               | s <- toList $ fmap ((*3) . sqrt) sigmas] ++ "]"

       showm xs sigmas = "measurement [\n" ++
         intercalate "\n" 
           [ "  " ++ show x ++ " +- " ++ show s ++ " -- " ++ (show covC)
               | x <- xs
               | covC <- toLists $ sigmas
               | s <- fmap ((*3) . sqrt) $ toList . takeDiag $ sigmas] ++ "\n]"

errorSize x s = error $  "Input length mismatch. length(x) = " ++ show (length x)
                      ++ ", but length(s) or one of its components isn't"


type M = (Double, Double)
(+-) :: Double -> Double -> M
(+-) = (,)

-- | Constructs a measurement from
--   list of values and 3sigma confidence intervals
measurement :: [M] -> Measurement
measurement xs = m (unzip xs)
  where m (a,b) = Measurement (fromList a) (diag bb)
         where bb = fromList $ fmap ((**2) . (/3)) b

-- | Constructs a measurement from list of values and their covariance matrix
measurementCov :: [Double]   -- ^ measurement
               -> [[Double]] -- ^ covariance matrix
               -> Measurement
measurementCov x sigmas | any (\y -> length y /= length x) sigmas
                          || (length sigmas /= length x) =
                              errorSize x sigmas
                        | otherwise = Measurement (fromList x) (fromLists sigmas)

-- | extracts covariance matrix from a measurement
getCovariance :: Measurement -> Mx Double
getCovariance (Measurement _ cov) = cov

-- | extracts values from a measurement
getValues :: Measurement -> Vec Double
getValues (Measurement val _)  = val

-- | predefined symbolic values to be used in defining expression
xs@[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] =
  map (Symbol.('x':).show) [1..11] :: [Fn]

-- | Smart constructor of nonlinear transformation
--   e.g. nt [x1*x1, x2, sin(x3)]
mkTransf :: [Fn]       -- ^ A list of functions, one for each output parameter
   -> Transf 
mkTransf fs = Nt fs1 (jacobian fs1)
  where
     fs1 = map simplify fs

defVar :: String -> Fn
defVar = var

partial (Nt fs fs') env = Nt (map (partEval env) fs) (map (map (partEval env)) fs')

-- | Calculates Jacobian matrix
jacobian :: [Fn] -> [[Fn]]
jacobian fs = transpose [map d fs | d <- ds]
  where
    ds = [ diff s | s <- xs']
    xs' = uniqSym $ concatMap variablesOf fs

variables :: Transf -> [Fn]
variables (Nt fs _) = uniqSym $
    concatMap variablesOf fs

uniqSym :: (Eq a, Ord a) => [Expr a] -> [Expr a]
uniqSym fs =
    nubBy ((==)     `on` getSym) $
    sortBy (compare `on` getSym) $ fs
  where
    getSym (Symbol a) = a
    getSym _  = error "not a symbol"

-- | Evaluates non-linear transformation at operation point
operatingPoint :: Transf -> [(Fn, Double)] -> (Vec Double, Mx Double)
operatingPoint   (Nt fs fs') env =
  (fromList  $ map (eval env) fs,
   fromLists $ map (map (eval env)) fs')

-- | Applies non-linear transformation to sample
apply :: Transf -> Measurement -> Measurement
apply nlt@(Nt fs _) (Measurement x mS) = Measurement f (mL >< mS >< trans mL)
  where (f,mL) = operatingPoint nlt (zip (variables nlt) $ toList x)


-- | Calculates calculation error from linearization of transformation
linError :: Transf -> Measurement -> Vec Double
linError nlt@(Nt fs j) (Measurement x mS) = 
  f1 - (f0 + (mL >. sigmas))
  where
    sigmas   = fmap sqrt $ takeDiag mS
    vars     = variables nlt
    x'       = x + sigmas
    (f0, mL) = operatingPoint nlt (zip vars $ toList x)
    (f1, _)  = operatingPoint nlt (zip vars $ toList x')    

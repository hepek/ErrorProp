{-# language ParallelListComp #-}
-- | Module 'ErrorProp' is used to calculate error propagation in
--   linear and non-linear systems
module Math.ErrorProp
       (Measurement
        , Fn
        , transf
        , (+-), measurement, measurementCov
        , covariance
        , variables, uniqSym
        , defVar
        , x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
        , transform
        , linearizationError
        , diag, takeDiag, trans, (>.), (><)
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

-- | Measurementment represented by measured value and corresponding
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
    | mSigma == mD = showv (toList x) (toList d)
    | otherwise    = showv (toList x) (toList . takeDiag $ mSigma)
    where
       d  = takeDiag mSigma
       mD = diag d

       showv xs sigmas = "measurement [\n" ++
         intercalate ",\n" 
           [ "  " ++ show x ++ " +- " ++ show (sqrt s)
               | x <- xs
               | s <- sigmas] ++ "]"
       showm xs sigmas =
         intercalate "\n"
           [ show x ++ "\t\t" ++ (unwords . intersperse "\t" $
                  map show line)
                    | x <- xs
                      | line <- sigmas]


errorSize x s = error $  "Input length mismatch. length(x) = " ++ show (length x)
                      ++ ", but length(s) or one of its components isn't"


type M = (Double, Double)
(+-) :: Double -> Double -> M
(+-) = (,)

-- | Measurement: constructs uncorrelated sample
measurement :: [M] -> Measurement
measurement xs = m (unzip xs)
  where m (a,b) = Measurement (fromList a) (diag (bb*bb))
         where bb = (fromList b)


-- | Measurement smart constructor: constructs correlated sample
measurementCov :: [Double]   -- ^ measurement
               -> [[Double]] -- ^ covariance matrix
               -> Measurement
measurementCov x sigmas | any (\y -> length y /= length x) sigmas
                          || (length sigmas /= length x) =
                              errorSize x sigmas
                        | otherwise = Measurement (fromList x) (fromLists sigmas)

covariance :: Measurement -> Mx Double
covariance (Measurement _ cov) = cov

-- | predefined symbolic values to be used in defining expression
xs@[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] =
  map (Symbol.('x':).show) [1..11] :: [Fn]

-- | Smart constructor of nonlinear transformation
--   e.g. nt [x1*x1, x2, sin(x3)]
transf :: [Fn]       -- ^ A list of functions, one for each output parameter
   -> Transf 
transf fs = Nt fs1 (jacobian fs1)
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

-- | evaluate non-linear transformation at operation point
operatingPoint :: Transf -> [(Fn, Double)] -> (Vec Double, Mx Double)
operatingPoint   (Nt fs fs') env =
  (fromList  $ map (eval env) fs,
   fromLists $ map (map (eval env)) fs')

transform :: Transf -> Measurement -> Measurement
transform nlt@(Nt fs _) (Measurement x mS) = Measurement f (mL >< mS >< trans mL)
  where (f,mL) = operatingPoint nlt (zip (variables nlt) $ toList x)


--linearizationError :: Transf -> Measurement -> Double
linearizationError nlt@(Nt fs j) (Measurement x mS) = 
  f1 - (f0 + (mL >. sigmas))
  where
    sigmas   = fmap sqrt $ takeDiag mS
    vars     = variables nlt
    x'       = x + sigmas
    (f0, mL) = operatingPoint nlt (zip vars $ toList x)
    (f1, _)  = operatingPoint nlt (zip vars $ toList x')    

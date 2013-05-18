module Math.SimpleMx where

import Data.List
import Data.List.Split
import Control.Applicative

-- | Matrix vector multiplication
-- (>.) :: (Num a) => Mx a -> Vec a -> Mx a
mA >. x  = map (muladd x) (mA)
   where
     muladd x y = sum (zipWith (*) x y)

-- | Matrix multiplication
(><) :: (Num a) => [[a]] -> [[a]] -> [[a]]
mA >< mB = transpose [ mA >. col | col <- transpose mB ]

addColum mX c = zipWith (++) mX (transpose [c])
addRow   mX c = mX ++ [c]

concatDiag :: (Num a) => [[a]] -> [[a]] -> [[a]]
concatDiag mX mY = 
  (zipWith (++) mX (replicate ll extR)) ++ (zipWith (++) (replicate lr extL) mY)
   where 
    ll   = length mX
    lr   = length mY
    extR = replicate lr 0
    extL = replicate ll 0

takeDiag :: [[a]] -> [a]
takeDiag m = [ m !! (i-1) !! (i-1) | i <- [1 .. length m]]

diag :: (Num a) => [a] -> [[a]]
diag d = chunksOf n $ intercalate zeros $ transpose [d]
  where
    zeros = replicate n 0
    n     = length d

trans :: [[a]] -> [[a]]
trans  = transpose

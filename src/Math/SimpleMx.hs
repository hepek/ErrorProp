module Math.SimpleMx where

import Data.List
import Data.List.Split

-- | Matrix vector multiplication
-- (>.) :: (Num a) => Mx a -> Vec a -> Mx a
mA >. x  = map (muladd x) (mA)
   where
     muladd x y = sum (zipWith (*) x y)

-- | Matrix multiplication
--(><) :: (Num a) => [[a]] -> [[a]] -> [[a]]
mA >< mB = transpose [ mA >. col | col <- transpose mB ]

addColum mX c = zipWith (++) mX (transpose [c])
addRow   mX c = mX ++ [c]

takeDiag :: [[a]] -> [a]
takeDiag m = [ m !! (i-1) !! (i-1) | i <- [1 .. length m]]

--diag :: (Num a) => [a] -> [[a]]
diag d = chunksOf n $ intercalate zeros $ transpose [d]
  where
    zeros = replicate n 0
    n     = length d

--trans :: [[a]] -> [[a]]
trans  = transpose


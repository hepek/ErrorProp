module Math.SimpleMx
    ( Vec, Mx
    , fromLists, toLists, fromList, toList
    , takeDiag, diag, trans
    , (><), (>.))
    where

import Data.List
import Data.List.Split
import Control.Applicative

newtype Vec a = Vec { toList  :: [a] }
              deriving (Eq,Show,Ord)

newtype Mx a = Mx [Vec a]
             deriving (Eq,Show,Ord)

fromLists xs = Mx $ map fromList xs
fromList  = Vec
toLists (Mx a) = map toList a

instance Functor Vec where
    fmap f (Vec a) = fromList (fmap f a)

instance Applicative Vec where
   pure a = Vec [a]
   (Vec f) <*> (Vec a) = Vec $ getZipList $ (ZipList f) <*> (ZipList a)

instance (Num a) => Num (Vec a) where
    a + b = (+) <$> a <*> b
    a * b = (*) <$> a <*> b
    a - b = (-) <$> a <*> b
    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum
    fromInteger = fromInteger

instance Functor Mx where
    fmap f (Mx a) = Mx (fmap (fmap f) a)

instance Applicative Mx where
    pure a = Mx [pure a]
    (Mx f) <*> (Mx a) = Mx $ getZipList $ (ZipList (fmap (<*>) f)) <*> (ZipList a)

instance (Num a) => Num (Mx a) where
    a + b = (+) <$> a <*> b
    a * b = (*) <$> a <*> b
    a - b = (*) <$> a <*> b
    negate = fmap negate        
    abs    = fmap abs
    signum = fmap signum
    fromInteger = fromInteger

sumVec (Vec a) = sum a

-- | Matrix vector multiplication
(>.) :: (Num a) => Mx a -> Vec a -> Vec a
(Mx mA) >. x = fromList $  map (muladd x) mA

muladd x y = sumVec (x * y)

-- -- | Matrix multiplication
(><) :: (Num a) => Mx a -> Mx a -> Mx a
(Mx mA) >< mB = fromLists $ chunksOf n $ [ muladd a b | a <- mA, b <- mB']
  where
    (Mx mB') = trans mB
    n        = length mA

takeDiag :: Mx a -> Vec a
takeDiag m = fromList $ [ m' !! (i-1) !! (i-1) | i <- [1 .. length m']]
   where m' = toLists m

diag :: (Num a) => Vec a -> Mx a
diag (Vec d) = fromLists $ chunksOf n $ intercalate zeros $ transpose [d]
  where
     zeros = replicate n 0
     n     = length d

trans :: Mx a -> Mx a
trans  = fromLists . transpose . toLists

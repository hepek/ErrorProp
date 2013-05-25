module Math.SimpleMx
    ( Vec, Mx
    , fromLists, toLists, fromList, toList
    , takeDiag, diag, trans
    , mxSize
    , (><), (>.))
    where

import Data.List
import Data.List.Split
import Control.Applicative


newtype Vec a = Vec { toList  :: [a] }
              deriving (Eq,Ord)

newtype Mx a = Mx [Vec a]
             deriving (Eq,Ord)

fromLists xs 
    | (length . nub . (fmap length)$  xs) /= 1 = error "different list sizes"
    | otherwise = Mx $ map fromList xs
fromList  = Vec
toLists (Mx a) = map toList a

instance (Show a) => Show (Vec a) where
   show = show . toList

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

instance (Show a) => Show (Mx a) where
    show (Mx a) = intercalate "\n" (fmap show a)

instance Functor Mx where
    fmap f (Mx a) = Mx (fmap (fmap f) a)

instance Applicative Mx where
    pure a = Mx [pure a]
    (Mx f) <*> (Mx a) = Mx $ getZipList $ (ZipList (fmap (<*>) f)) <*> (ZipList a)

instance (Num a) => Num (Mx a) where
    a + b = (+) <$> a <*> b
    a * b = (*) <$> a <*> b
    a - b = (-) <$> a <*> b
    negate = fmap negate        
    abs    = fmap abs
    signum = fmap signum
    fromInteger = fromInteger

sumVec (Vec a) = sum a

-- | Matrix vector multiplication
(>.) :: (Num a) => Mx a -> Vec a -> Vec a
(Mx mA) >. x@(Vec xs)
    | (snd (mxSize $ Mx mA)) /= (length xs) = error "size mismatch"
    | otherwise = fromList $  map (muladd x) mA

muladd x y = sumVec (x * y)

mxSize (Mx a) = (length a, b a)
  where
    b a | (length $ nub s) /= 1 = error "bad matrix"
        | otherwise = head s
     where s = fmap (length . toList) a

sizeError a b = error $ "size mismatch: " ++ (show $mxSize a) ++( show $mxSize b)

infixl 4 ><

-- -- | Matrix multiplication
(><) :: (Num a) => Mx a -> Mx a -> Mx a
a@(Mx mA) >< b@(Mx mB) 
    | (snd . mxSize $ a) /= (fst . mxSize $ b)  = sizeError a b
    | otherwise = 
  trans .  fromLists . (chunksOf m) $ [ muladd a b |  b <- mB', a <- mA]
  where
    (Mx mB') = trans b
    (m,n)    = mxSize a

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

mxA = fromLists [[1,2,3],[4,5,6]]
mxB = fromLists [[1,2],[3,4]]

--prop_diag v = takeDiag (diag v) == v

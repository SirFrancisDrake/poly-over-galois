
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}

import Control.Monad.Reader
import Math.Algebra.Field.Base
import Math.Polynomial
-- #define F2 <anyField>
type PolyOverGaloisPolynomial = Poly (Poly F2)
type PolyOverGaloisNumerical = Poly NC

-- #define F2 <-someField->
data (Num a) => Field a = Field
    { factor :: Poly a
    }
    deriving (Show)

data NC = NC { ni :: Int }
    deriving (Eq)

instance Show NC where
    show (NC a) = show a

toNC :: (Num a) => Int -> Reader (Field a) NC
toNC i = do
    f <- ask
    let factorP = factor f
    return $ NC i

--     ncs <- ncl
--     if (NC i) `elem` ncs
--         then return $ NC i
--         else error $ "number " ++ show i ++ "does not exist in GF(" 
--                      ++ show (length ncs) ++ ")"


--     ask >>= factor >>= \factorPoly ->
--     if (NC i) `elem` (ncl factorPoly)
--         then return $ NC i
--         else error $ "number " ++ show i ++ "does not exist in GF(" 
--                      ++ show (length $ ncl factorPoly) ++ ")"

ncl :: (Num a) => Reader (Field a) [NC]
ncl = do
    f <- ask
    let factorPoly = factor f
    let factorPower = length (polyCoeffs LE factorPoly) - 1
    let powerOfSet = 2^factorPower - 1
    return $ map (\i -> NC i) [0..powerOfSet]
-- 
findReverse :: NC -> Poly F2 -> NC
findReverse a = head $ filter (\t -> a * t == 1) ncl

gcdNC :: Poly NC -> Poly NC -> Poly NC
gcdNC a b
    | polyIsZero b = a
    | otherwise = gcdNC b (a `remPoly` b)
-- 
-- gcdNCExt :: Poly NC -> Poly NC -> (Poly NC,Poly NC,Poly NC)
-- gcdNCExt a b = let g = gcdPoly a b
--                    (p,q) = gcdPoly' a b
--                in (g,p,q)
-- 
-- gcdNCExtNotNorm :: Poly NC -> Poly NC -> (Poly NC,Poly NC,Poly NC)
-- gcdNCExtNotNorm a b = 
--     let g = gcdNC a b
--         (p,q) = gcdPoly' a b
--     in (g,p,q)
-- 
-- gcdPoly' :: (Num a, Fractional a) => Poly a -> Poly a -> (Poly a, Poly a)
-- gcdPoly' a b
--     | polyIsZero b = (poly LE [1], poly LE [])
--     | otherwise = 
--         let (q, r) = (quotPoly a b, remPoly a b)
--             (s, t) = gcdPoly' b r
--         in (t, addPoly s (negatePoly $ multPoly q t))
-- 
-- instance Num NC where
--     (NC a) + (NC b) = NC $ reconv $ addPoly (conv a) (conv b)
--     (-) = (+)
--     (NC a) * (NC b) = NC $ reconv $ dothemath
--                       where dothemath = remPoly (multPoly (conv a) (conv b))
--                                                 factorPoly
--     negate = id
--     abs = id
--     signum _ = NC 1
--     fromInteger i = NC (fromInteger i)
-- 
-- instance Fractional NC where
--    fromRational = error "111" 
--    (/) a = (*a) . findReverse
-- 
-- instance Fractional (Poly F2) where
--    fromRational = error "111" 
--    (/) = quotPoly
-- 
-- instance Num (Poly F2) where
--     (+) = addPoly
--     (-) = (+)
--     (*) = multPoly
--     negate = id
--     abs = id
--     signum = id
--     fromInteger _ = error "wtf"
-- 
-- toInt :: F2 -> Int
-- toInt 0 = 0
-- toInt 1 = 1
-- 
-- fromInt :: Int -> F2
-- fromInt 0 = 0
-- fromInt 1 = 1
-- 
-- conv :: Int -> Poly F2
-- conv i = conv' i []
-- 
-- conv' :: Int -> [F2] -> Poly F2
-- conv' 0 cs = poly LE cs
-- conv' i cs = conv' (i `div` 2) (cs ++ [fromInt $ i `mod` 2])
-- 
-- reconv :: Poly F2 -> Int
-- reconv p = foldr (\c acc -> acc*2 + toInt c) 0 (polyCoeffs LE p)

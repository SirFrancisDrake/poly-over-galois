{-# LANGUAGE FlexibleInstances #-}

module Galois where

import Data.List (intersperse)
import Math.Algebra.Field.Base
import Math.Polynomial

type PolyOverGaloisPolynomial = Poly (Poly F2)
type PolyOverGaloisNumerical = Poly NC

data NC = NC { ni :: Int }
    deriving (Eq)

instance Show NC where
    show (NC a) = show a

factorPoly :: Poly F2 -- ебало, по которому факторизуем
           -- степени  0 1 2 3 4 5
factorPoly = poly LE [1,0,1,0,0,1]

toNC :: Int -> NC
toNC i = if i > 31 then error $ show i ++ " is too large to be in GF(32)"
                   else NC i

ncl :: [NC]
ncl = map toNC [0..powerOfSet]
      where powerOfSet = 2^factorPower - 1
            factorPower = length (polyCoeffs LE factorPoly) - 1

findReverse :: NC -> NC
findReverse a = head $ filter (\t -> a * t == 1) ncl

gcdNC :: Poly NC -> Poly NC -> Poly NC
gcdNC a b
    | polyIsZero b = a
    | otherwise = gcdNC b (a `remPoly` b)

gcdNCExt :: Poly NC -> Poly NC -> (Poly NC,Poly NC,Poly NC)
gcdNCExt a b = let g = gcdPoly a b
                   (p,q) = gcdPoly' a b
               in (g,p,q)

gcdNCExtNotNorm :: Poly NC -> Poly NC -> (Poly NC,Poly NC,Poly NC)
gcdNCExtNotNorm a b = 
    let g = gcdNC a b
        (p,q) = gcdPoly' a b
    in (g,p,q)

gcdPoly' :: (Num a, Fractional a) => Poly a -> Poly a -> (Poly a, Poly a)
gcdPoly' a b
    | polyIsZero b = (poly LE [1], poly LE [])
    | otherwise = 
        let (q, r) = (quotPoly a b, remPoly a b)
            (s, t) = gcdPoly' b r
        in (t, addPoly s (negatePoly $ multPoly q t))

instance Num NC where
    (NC a) + (NC b) = NC $ reconv $ addPoly (conv a) (conv b)
    (-) = (+)
    (NC a) * (NC b) = NC $ reconv $ dothemath
                      where dothemath = remPoly (multPoly (conv a) (conv b))
                                                factorPoly
    negate = id
    abs = id
    signum _ = NC 1
    fromInteger i = NC (fromInteger i)

instance Fractional NC where
   fromRational = err_never
   (/) a = (*a) . findReverse

instance Fractional (Poly F2) where
   fromRational = err_never 
   (/) = quotPoly

instance Num (Poly F2) where
    (+) = addPoly
    (-) = (+)
    (*) = multPoly
    negate = id
    abs = id
    signum = id
    fromInteger _ = err_never

toInt :: F2 -> Int
toInt 0 = 0
toInt 1 = 1

fromInt :: Int -> F2
fromInt 0 = 0
fromInt 1 = 1

conv :: Int -> Poly F2
conv i = conv' i []

conv' :: Int -> [F2] -> Poly F2
conv' 0 cs = poly LE cs
conv' i cs = conv' (i `div` 2) (cs ++ [fromInt $ i `mod` 2])

reconv :: Poly F2 -> Int
reconv p = foldr (\c acc -> acc*2 + toInt c) 0 (polyCoeffs LE p)

class Ppr a where
    ppr_show :: a -> String
    ppr_print :: a -> IO ()
    ppr_print = putStrLn . ppr_show

instance Ppr (Poly NC) where
    ppr_show a = powers
           where powers = if polyIsZero a 
                           then "0"
                           else (show $ head $ (polyCoeffs LE a)) ++ " + " ++ otherPowers
                           where otherPowers = concat $ intersperse " + " $ zipWith (++)
                                                (map show $ tail $ (polyCoeffs LE a))
                                                ["*x^" ++ show t | t <- [1..(length $ polyCoeffs LE a)]]

err_never = error "This should have never come up"

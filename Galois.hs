{-# LANGUAGE FlexibleInstances #-}

module Galois where

import Control.Monad (forM_)
import qualified Control.Monad.State as S
import Data.Function (on)
import Data.List (intersperse, sortBy)
import Math.Algebra.Field.Base
import Math.Polynomial

type PolyOverGaloisPolynomial = Poly (Poly F2)
type PolyOverGaloisNumerical = Poly NC

data NC = NC { ni :: Int }
    deriving (Eq)

instance Show NC where
    show (NC a) = show a

factorPoly :: Poly F2 -- ебало, по которому факторизуем
           -- степени 0 1 2 3 4 5
factorPoly = poly LE [1,0,0,1,1]

-- ВРЕМЕННЫЙ КОД УДАЛИТЬ ПО ЗАВЕРШЕНИЮ НАЧАЛО
-- D =
type D = (NC -> NC, NC -> NC)

_D :: D
_D =
  ( \x -> 1 -- a
  , \x -> 0     -- b
  )

-- D' =
_D' :: D
_D' =
  ( \x -> 1       -- a
  , \_ -> 0           -- b
  )

-- D - D' =
_D_D' :: D
_D_D' = 
  ( \x -> 1       -- a
  , \_ -> 0           -- b
  )

a :: D -> NC -> NC
a  = \_ _ -> 1

b :: D -> NC -> NC
b  = \_ _ -> 0

b' :: D -> NC -> NC
b' d x = (b d) x + 1

f1 :: D -> NC -> NC -> NC
f1 p x y = ((b' p) x - y) / ((a p) x)

f2 :: D -> NC -> NC -> NC
f2 _ _ _ = 1

s1 :: [ (NC, NC) ]
s1 = 
  [ (0, 8)
  , (0, 9)
  , (3, 14)
  , (6, 12)
  , (6, 13)
  , (7, 6)
  , (7, 7)
  , (8, 14)
  , (9, 10)
  , (9, 11)
  , (10, 8)
  , (10, 9)
  , (11, 14)
  , (11, 15)
  , (13, 4)
  , (13, 5)
  ]

--ppr :: [ [NC] ] -> String
--ppr ss = concatMap (\a -> (foldl (\acc n -> acc ++ show n ++ " ") "" a) ++ "\n") ss

ppr :: [ [NC] ] -> IO ()
ppr ss = putStrLn $ 
  concatMap (\a -> (foldl (\acc n -> acc ++ show n ++ " ") "" a) ++ "\n") ss

pow :: Int -> NC -> NC
pow 0 _ = 1
pow i x = (pow (i - 1) x) * x

mlim :: D -> (D -> NC -> NC -> NC) -> (Int, Int) -> [ NC -> NC -> NC ]
mlim d f (a, b) = [a..b] >>= \t -> [ \x y -> (f d x y) * (pow t x) ]

tmpsmall x y = [f1 _D x y, (f1 _D x y) * x, 1, x, x^2, x^3, x^4, (y + 1) * x^2]

fs f = map (\fn -> map (uncurry fn) s1) (mlim _D f (0, 3))

dmatrix = fs Galois.f1 ++ fs Galois.f2

matrVecMult :: [[NC]] -> [NC] -> [NC]
matrVecMult matr vec =
  map (\str -> sum $ zipWith (*) str vec) matr

orthog :: [[NC]] -> [NC] -> Bool
orthog matr vec = (matrVecMult matr vec) == (replicate (length matr) 0)

end = [1, 0, 0, 0, 0, 0, 0, 0]

r0 :: [a] -> [[a]]
r0 ls = ls >>= \x -> map (\t -> [x,t]) ls

ri :: [a] -> [[a]] -> [[a]]
ri bs es = concatMap (\e -> map (\b -> b:e) bs) es

repl 0 ls = []
repl 1 ls = [ls]
repl 2 ls = r0 ls
repl i ls = ri ls (repl (i-1) ls)

findVec :: [NC]
findVec = head $ filter (orthog dmatrix) (map (++end) (repl 8 ncl))

addMultBy :: [NC] -> [NC] -> NC -> [NC]
addMultBy l1 l2 n = zipWith (\a  b -> a + b * n) l1 l2

normalize :: [NC] -> [NC]
normalize l =
  let n = head $ filter (/= 0) l
  in  map (*(1/n)) l

findNum :: (Eq a) => [a] -> a -> Int
findNum ls l =
  fst $ head $ filter (\(p,q) -> q == l) (zip [1..] ls)

findPermutation :: (Eq a) => [a] -> [a] -> [Int]
findPermutation lb la = map (findNum lb) la

firstNotNil l = fst $ head $ filter (\(p,q) -> q /= 0) (zip [0..] l)

firstStep :: [[NC]] -> [ (Int, [NC]) ]
firstStep lls =
  let second ls = map snd (sortBy (on compare fst) (map (\l -> (firstNotNil l, l)) ls))
  in  zip (findPermutation lls $ second lls) (second lls)

killN :: [[NC]] -> Int -> [[NC]]
killN ls n = 
  let nstr = ls !! n
  in  map (\l -> 
        if (on (==) firstNotNil l nstr) && l /= nstr
          then
            let n1 = nstr !! (firstNotNil nstr)
                n2 = l !! (firstNotNil l)
            in  addMultBy l nstr (n2/n1)
          else l
          )
          ls

type TM a = S.State [[NC]] a

solve :: Int -> TM ()
solve i =
  forM_ [0..i] (\i -> S.get >>= return . (flip killN i) >>= S.put)

sodomizeWith :: [NC] -> [[NC]] -> [[NC]]
sodomizeWith constr matr =
  let wheresUnity = fst $ head $ filter (\(a,b) -> b /= 0) (zip [0..] constr)
  in  map (\(ind,s) -> 
        if s /= constr
          then addMultBy s constr (s !! wheresUnity) 
          else constr)
       (zip [0..] matr)

sodomize :: Int -> TM ()
sodomize i =
  let fn i matr = return $ sodomizeWith (matr !! i) matr
  in  forM_ [0..i] (\i -> S.get >>= fn i >>= S.put)

-- [0,1] -> [[2,3],[4,5]] = [[0,2,3],[1,2,3],[0,4,5],[1,4,5]]

-- ВРЕМЕННЫЙ КОД УДАЛИТЬ ПО ЗАВЕРШЕНИЮ КОНЕЦ

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

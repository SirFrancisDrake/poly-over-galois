{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}

import Control.Monad (mapM_, foldM_)
import Data.Function (on)
import Data.List ( (\\), intersperse, sort, sortBy)
import Math.Algebra.Field.Base hiding (char)
import Math.Polynomial
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

run :: Show a => Parser a -> String -> IO ()
run p input
    = case (parse p "" input) of
        Left err -> putStr "parse error at " >> print err
        Right x -> print x

run' p input
    = case (parse p "" input) of
        Left err -> putStr "parse error at " >> print err
        Right x -> putStrLn $ showP x

expr :: Parser PolyOverGaloisNumerical
expr = buildExpressionParser table factor
       <?> "expression"

table = [[op "*" multPoly AssocLeft
         , op "div" quotPoly AssocLeft
         , op "mod" remPoly AssocLeft]
        ,[op "+" addPoly AssocLeft
         ,op "-" addPoly AssocLeft]]
        where op s f assoc
                = Infix (try $ skipMany (char ' ') >> string s >> skipMany (char ' ') >> return f) assoc

factor = do{ char '('
           ; x <- polyP
           ; char ')'
           ; return x
           }
        <?> "simple expression"

getPolynomial :: IO PolyOverGaloisNumerical
getPolynomial = getLine >>= \a ->
                case (parse polyP "" a) of
                    Left err -> getPolynomial
                    Right x -> return x

polyGet :: String -> PolyOverGaloisNumerical
polyGet input = case (parse polyP "" input) of
                    Left err -> poly LE [137]
                    Right x -> x

monoP = (try $ do {c <- coeffP; skipMany (char ' '); char 'x'; p <- powP; return (c,p) })
        <|> (try (char '-' >> skipMany (char ' ') >> char 'x' >> powP >>= \p -> return ((-1,p))))
        <|> (try (char '+' >> skipMany (char ' ') >> char 'x' >> powP >>= \p -> return (1,p)))
        <|> (char 'x' >> skipMany (char ' ') >> powP >>= \p -> return (1,p))
        <|> (char '-' >> skipMany (char ' ') >> intP >>= \i -> return (i * (-1),0))
        <|> (char '+' >> skipMany (char ' ') >> intP >>= \i -> return (i,0))
        <|> (intP >>= \i -> return (i,0))
        <?> "monomial"

exparse :: Parser PolyExpr
exparse = do
    a <- polyParP
    skipMany (char ' ')
    fn <- fnP
    skipMany (char ' ')
    b <- polyParP
    return (fn,a,b)

polyParP :: Parser PolyOverGaloisNumerical
polyParP = do
    char '('
    p <- polyP
    char ')'
    return p

type PolyExpr = (PolyFN, PolyOverGaloisNumerical, PolyOverGaloisNumerical)

fnP :: Parser PolyFN
fnP = (char '+' >> return Plus)
      <|> (char '-' >> return Minus)
      <|> (char '*' >> return Times)
      <|> (string "div" >> return Quot)
      <|> (string "mod" >> return Rem)
      <|> (string "gcd" >> return GCD)
      <|> (string "gcdnn" >> return GCD1)
      <?> "binary_operation"

data PolyFN = Plus
            | Minus
            | Times
            | Quot
            | Rem
            | GCD
            | GCD1
    deriving (Eq, Show)

kill :: String -> String
kill str =
    let (fn,a,b) = case parse exparse "" str of
                            Left _ -> error "failed parse"
                            Right (a,b,c) -> (a,b,c)
    in process fn a b

process :: PolyFN -> PolyOverGaloisNumerical -> PolyOverGaloisNumerical -> String
process fn a b =
    case fn of
        Plus -> showP $ addPoly a b
        Minus -> showP $ addPoly a b
        Times -> showP $ multPoly a b
        Quot -> showP $ quotPoly a b
        Rem -> showP $ remPoly a b
        GCD -> let (x,y,z) = gcdNCExt a b
               in "(" ++ showP x ++ ", " ++ (showP y) ++ ", "++ (showP z) ++ ")"
        GCD1 -> let (x,y,z) = gcdNCExtNotNorm a b
                in (showP x) ++ (showP y) ++ (showP z)

monoPSpaced = do
    m <- monoP
    skipMany (char ' ')
    return m

coeffP = (do {char '+'; skipMany (char ' '); i <- intP; return i;})
         <|> (do {i <- intP; return i;})
         <|> (do {char '-'; i <- intP; return (i * (-1));})
         <?> "coefficient"

powP = (try (char '^') >> intP)
       <|> (return 1)
       <?> "power"

intP = (many1 digit >>= return . read)
       <|> (do char '('
               i <- (intP <|> (char '-' >> intP >>= \i -> return ((-1)*i)))
               char ')' 
               return i )
       <?> "number"

polyP :: Parser PolyOverGaloisNumerical
polyP = let powers ps = map snd ps
            otherPowers ps = [0..maximum (powers ps)] \\ (powers ps)
            otherPairs ps = map (\t -> (0,t) ) (otherPowers ps)
            pairs m = m ++ (otherPairs m)
            process m = poly LE ( map toNC $ map fst (sortBy (on compare snd) $ pairs m) )
        in (many1 monoP >>= \m -> return $ process m)

type PolyOverGaloisPolynomial = Poly (Poly F2)
type PolyOverGaloisNumerical = Poly NC

data NC = NC { ni :: Int }
    deriving (Eq)

instance Show NC where
    show (NC a) = show a

factorPoly :: Poly F2 -- ебало, по которому факторизуем
           -- степени  0 1 2 3 4 5
factorPoly = poly LE [1,0,1,0,0,1]

-- Генерирует все принципиальные многочлены степени до i
-- В сущности никому не нужно
-- genSubsets :: Int -> [[Int]]
-- genSubsets 1 = [[0],[1]]
-- genSubsets i =
--     let f a = [0:a, 1:a]
--     in genSubsets (i-1) >>= f
--
-- foldl (\
-- 
-- genPolys :: Int -> [Poly Int]
-- genPolys i = map (poly LE) (genSubsets i)

toNC :: Int -> NC
toNC i = if i > 31 then error "ya tvoju mamu etc"
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
   fromRational = error "111" 
   (/) a = (*a) . findReverse

instance Fractional (Poly F2) where
   fromRational = error "111" 
   (/) = quotPoly

instance Num (Poly F2) where
    (+) = addPoly
    (-) = (+)
    (*) = multPoly
    negate = id
    abs = id
    signum = id
    fromInteger _ = error "wtf"

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

pcycle :: IO ()
pcycle = do
    putStr "zzz> "
    a <- getLine
    putStrLn (kill a)
    pcycle

showM c p =
    let showc = case c of
            1 -> "1"
            otherwise -> show c 
        showp = case p of
            0 -> ""
            1 -> "x"
            otherwise -> "x^" ++ show p
    in showc ++ showp

showP p = 
    let xs = polyCoeffs LE p
        fnx (p,c) = if (p == 0) && (c == 0) then ""
                                            else showM c p
        fn = map fnx (zip [0..] xs)
    in if xs == [] then "0"
                   else concat $ intersperse " + " (filter (/= "") fn)

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}

import Control.DeepSeq
import Control.Exception hiding (try)
import Control.Monad (mapM_, foldM_)
import Data.List (intersperse)
import Data.Maybe (fromJust, isJust)
import Math.Polynomial
import System.Console.Readline
import Text.ParserCombinators.Parsec

import Galois
import PolyParser

expr :: String -> String
expr e =
    case parse (try gcdExpr) "" e of
        Left err1 -> 
            case parse (try arExpr) "" e of
                Left err2 -> "failed to parse " ++ show err1 ++ " " ++ show err2
                Right res -> showP res
        Right (a,b,r) -> "(" ++ showyP a ++ " ,   " ++ showzP b ++ " ,   " ++ showzP r ++ ")"
        where showyP a = if showP a == "" then "1" else showP a
              showzP a = if showP a == "" then "1" else showP a

pcycle :: IO ()
pcycle = do
    sequence_ [putStr "zzz> "]
    a <- readline "" >>= return . fromJust
    case a of
        "q" -> return ()
        "h" -> putStrLn msg_help >> pcycle
        otherwise -> putStrLn (expr a) >> pcycle 

msg_help = "\n==> Basic expression form: (polynomial) operation (polynomial)" ++
           "\n==> example: (x^3) + (x^2 + 1)" ++
           "\n==> operations: + * div mod gcd gcdnn" ++
           "\n==> gcdnn stands for `not normalised gcd`" ++
           "\n==> h gives help, q quits the program\n"

showM c p =
    let showc = case c of
            1 -> if p == 0 then "1"
                           else ""
            otherwise -> show c 
        showp = case p of
            0 -> ""
            1 -> "x"
            otherwise -> "x^" ++ show p
    in showc ++ showp

showP p = 
    let xs = polyCoeffs LE p
        fnx (p,c) = if (c == 0) then ""
                                else showM c p
        fn = map fnx (zip [0..] xs)
    in if xs == [] then "0"
                   else concat $ intersperse " + " (filter (/= "") fn)

main = pcycle

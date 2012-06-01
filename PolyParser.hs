
module PolyParser where

import Control.Applicative ( (<$>) )
import Data.Function (on)
import Data.List ( (\\), sortBy )
import Math.Polynomial
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Galois

arExpr :: Parser PolyOverGaloisNumerical
arExpr = buildExpressionParser table factor
       <?> "expression"

table = [[op "*" multPoly AssocLeft
         , op "div" quotPoly AssocLeft
         , op "mod" remPoly AssocLeft
         ]
        ,[op "+" addPoly AssocLeft
         ]]
        where op s f assoc
                = Infix (try $ skipMany (char ' ') >> string s >> skipMany (char ' ') >> return f) assoc

factor = do{ char '('
           ; x <- polyP
           ; char ')'
           ; return x
           }
        <?> "(polynomial)"

gcdExpr :: Parser ( PolyOverGaloisNumerical
                  , PolyOverGaloisNumerical
                  , PolyOverGaloisNumerical )
gcdExpr = do
    a <- polyParP
    skipMany (char ' ')
    fn <- fnP
    skipMany (char ' ')
    b <- polyParP
    case fn of
        GCD  -> return $ gcdNCExt a b
        GCD1 -> return $ gcdNCExtNotNorm a b

polyParP :: Parser PolyOverGaloisNumerical
polyParP = do
    char '('
    p <- polyP
    char ')'
    return p

type PolyExpr = (PolyFN, PolyOverGaloisNumerical, PolyOverGaloisNumerical)

fnP :: Parser PolyFN
fnP = (try (string "gcdnn") >> return GCD1)
      <|> (string "gcd" >> return GCD)
      <?> "gcd/gcdnn"

data PolyFN = GCD
            | GCD1
    deriving (Eq, Show)

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

monoP = (try $ do {c <- coeffP; skipMany (char ' '); char 'x'; p <- powP; return (c,p) })
        <|> (try (char '-' >> skipMany (char ' ') >> char 'x' >> powP >>= \p -> return ((-1,p))))
        <|> (try (char '+' >> skipMany (char ' ') >> char 'x' >> powP >>= \p -> return (1,p)))
        <|> (char 'x' >> skipMany (char ' ') >> powP >>= \p -> return (1,p))
        <|> (char '-' >> skipMany (char ' ') >> intP >>= \i -> return (i * (-1),0))
        <|> (char '+' >> skipMany (char ' ') >> intP >>= \i -> return (i,0))
        <|> (intP >>= \i -> return (i,0))
        <?> "monomial"

monoPSpaced = do
    m <- monoP
    skipMany (char ' ')
    return m

polyP :: Parser PolyOverGaloisNumerical
polyP = let powers ps = map snd ps
            otherPowers ps = [0..maximum (powers ps)] \\ (powers ps)
            otherPairs ps = map (\t -> (0,t) ) (otherPowers ps)
            pairs m = m ++ (otherPairs m)
            process m = poly LE ( map toNC $ map fst (sortBy (on compare snd) $ pairs m) )
        in (many1 monoPSpaced >>= \m -> return $ process m)

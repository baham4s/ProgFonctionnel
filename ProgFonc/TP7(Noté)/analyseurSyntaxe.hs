-- DUBIN Baptiste

import Data.Char
import Control.Applicative

data Parser a = P (String -> [(a, String)])

-- L'ANALYSE ET LE PREMIER ANALYSEUR
-- Retourne un parser
item :: Parser Char 
item = P (\s -> case s of 
                    [] -> [] 
                    x:xs -> [(x,xs)])
 
-- Résultat du parser appliqué a la chaîne
parse :: Parser a -> String -> [(a, String)]
parse (P a) s = a s



-- APPLIQUER DES ANALYSEUR EN SEQUENCE
-- Applique fonction unaire sur le contenu d'un premier parser
instance Functor Parser where 
    -- fmap :: (a -> b) -> Parser a -> Parser b 
    fmap f p = P (\s -> case parse p s of 
                            [] -> []
                            [(r, s')] -> [(f r, s')])

-- Applique cette fonction qui est le résultat de l’application d’un Parser et d’obtenir un nouveau Parser.
instance Applicative Parser where 
    -- pure :: a -> Parser a 
    pure a = P (\s->[(a,s)])
     
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b 
    pf <*> p = P (\s -> case parse pf s of 
                          [] -> [] 
                          [(f, s')] -> parse (fmap f p) s')

-- Applique en séquence des parser
instance Monad Parser where 
    -- return :: a -> Parser a 
    return = pure 
     
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b 
    p >>= f = P (\s -> case parse p s of 
                           [] -> []
                           [(r, s')] -> parse (f r) s')

-- Analyse les trois premiers caractères d’une chaîne 
three :: Parser (Char,Char) 
three = item >>= (\c -> item >>= (\_ -> item >>= (\e -> pure (c,e))))

-- APPLIQUER ALTERNATIVEMENT DES ANALYSEURS
-- Analyseur alternatif
instance Alternative Parser where 
    -- empty :: Parser a 
    empty = P (\s -> [])
 
    -- (<|>) :: Parser a -> Parser a -> Parser a 
    p1 <|> p2 = P (\s -> case parse p1 s of 
                        [] -> parse p2 s
                        [(r, s')] -> [(r,s')])

-- PARSER PRIMITIF
-- Marche si le premier caractère vérifie un prédicat donné en paramètre et qui échoue sinon
sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
	   if p c 
	   	then return c 
		else empty

-- Parse les chiffres	
digit :: Parser Char
digit = sat isDigit

-- Parse les lettres minuscules
lower :: Parser Char
lower = sat isLower

-- Parse les lettres majuscules
upper :: Parser Char
upper = sat isUpper

-- Parse toute les lettres
letter :: Parser Char
letter = sat isLetter

-- Parse les lettres et les chiffres
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- Parse le caractère donnée en argument
char :: Char -> Parser Char
char c = sat (== c)

-- Marche si le motif donné en paramètre correspond au début de la chaîne à analyser
string :: String -> Parser String 
string [] = return [] 
string (x:xs) = do char x 
                   string xs 
                   return (x:xs)

-- Essaie de parser un identifiant
ident :: Parser String 
ident = do 
	x <- lower
	xs <- many alphanum 
	return (x:xs)

-- Essaie de parser un nombre naturel (entier positif ou nul) comportant un ou plusieurs chiffres
nat :: Parser Int
nat = do 
	x <- some digit
	return (read x)
	
-- Essaie de parser les caractères d’espaces
space :: Parser ()
space = do
	many (sat isSpace)
	return ()
	
-- Essaie de parser un nombre entier
int :: Parser Int 
int = do char '-' 
         n <- nat 
         return (-n) 
      <|> nat 

-- expr ::= term ’+’ expr | term 
--  term ::= factor ’*’ term | factor 
--  factor ::= ’(’ expr ’)’ | nat 
--  nat ::= 0 | 1 | 2 | 3 | ... 

-- ANALYSE SYNTAXIQUE D'EXPRESSION ARITHMÉTIQUE
-- Parse une expression
expr :: Parser Int
expr = 
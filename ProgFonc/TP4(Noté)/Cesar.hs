-- DUBIN Baptiste
 
import Data.Char

-- Passage d'un char a la valeur ASCII
let2int:: Char -> Int
let2int c = ord c

-- Passage d'un entier a une valeur ASCII
int2let:: Int -> Char
int2let c = chr c

-- Décalage d'un caractère 
shift:: Int -> Char -> Char
shift n c | isUpper c && ((let2int c) + n) < let2int 'A' = int2let((let2int c) + (n `mod` 26))
          | isUpper c && ((let2int c) + n) >= let2int 'Z' = int2let((let2int c) + n - 26)
          | isLower c = shift n (toUpper c)
          | otherwise = int2let ((let2int c) + n)

-- Chiffrement du message
cypher:: Int -> String -> String
cypher n [] = []
cypher n xs = [shift n c | c <- xs, isLetter c]

-- Table de fréquence d'apparition des lettres
table::[Float]
table = [9.42, 1.02, 2.64, 3.39, 15.87, 0.95, 1.04, 0.77, 8.41, 0.89, 0.001, 5.34, 3.24, 7.15, 5.14, 2.86, 1.06, 7.9, 7.26, 6.24, 2.15, 0.001, 0.3, 0.24, 0.32]

-- Calcul pourcentage d'un entier par rapport a un autre entier
percent :: Int -> Int -> Float
percent a b = ((fromIntegral a / fromIntegral b)*100)

-- Compte le nombre d'occurence d'un caractère dans une chaine
count :: Char -> String -> Int
count c cs = length [d | d <- cs, c==d]

-- Obtenir les fréquences d'apparitions des lettres dans une chaîne
freqs:: String -> [Float]
freqs i = [percent (count c i) (length i) | c <- ['A'..'Z']]

-- Calcul la valeur khi-deux
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- Permet d'effectuer des rotations n fois
rotate:: Int -> [a] -> [a]
rotate x [] = []
rotate x m = drop x m ++ take x m

-- Affiche les positions d'un élément donnée
position:: Eq a => a -> [a] -> [Int]
position x xs = [ i | (x, i) <- zip xs [0..n], x==x ]
                 where n = length xs - 1

-- Décode une chaîne donnée
crack :: String -> String
crack xs = cypher (-factor) xs
      where factor = head(position(minimum chitab)chitab)
            chitab = [chisqr (rotate n table') table | n <- [0..25]]
            table' = freqs xs

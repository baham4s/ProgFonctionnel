-- import Prelude hiding (nom_fct)
-- Masque les Fonctions de bases

product_bis :: [Int] -> Int
product_bis [] = 1
product_bis(x : xs) = x * (product_bis xs)

length_bis :: [Int] -> Int
length_bis [] = 0
length_bis(x : xs) = 1 + length_bis xs

init_bis :: [a] -> [a]
init_bis [] = []
init_bis [x] = []
init_bis(x : xs) = [x] ++ init_bis xs

(+++) :: [a] -> [a] -> [a]
(+++) [] b = b
(+++) a [] = a
(+++) (x : xs) ys = x : xs +++ ys

insert_bis :: Int -> [Int] -> [Int]
insert_bis _ [] = []
insert_bis a (x : xs) = if a >= head xs
                          then x : insert_bis a xs          -- Penser a garder la liste lors de l'appel récursif
                          else x : a : xs                   -- Insérer entre liste pred et liste supp

isort :: [Int] -> [Int]
isort [] = []
isort(x : xs) = insert x (isort xs)
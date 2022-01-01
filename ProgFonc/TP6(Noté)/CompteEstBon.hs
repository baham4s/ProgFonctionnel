-- DUBIN Baptiste
-- Opérations arithmétiques autorisées
data Op  = Add | Sub | Mul | Div
     deriving Eq

-- Affichage des opérations en string
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- Test si l'opérations est valide
valid:: Op -> Int -> Int -> Bool
valid o x y | o == Add || (o == Sub  && x >= y) || o == Mul || (o == Div && x >= y && y /= 0) = True
            | otherwise = False

-- Applique l'opérations
apply:: Op -> Int -> Int -> Int
apply Add x y = x+y
apply Sub x y = x-y
apply Mul x y = x*y
apply Div x y = div x y



-- Type des expressions numéraires
data Expr = Val Int | App Op Expr Expr

-- Affichage des expressions
instance Show Expr where
    show (Val x) = show x
    show (App op exp1 exp2) | op == Add || op == Sub = (show exp1) ++ (show op) ++ (show exp2)
                            | otherwise = "("++(show exp1) ++ (show op) ++ (show exp2)++")"

-- Retourne les valeurs dans une liste
values:: Expr -> [Int]
values (Val x) = [x]
values (App op exp1 exp2) = (values exp1) ++ (values exp2)

-- Retourne une liste avec la valeur finale ou vide
eval:: Expr -> [Int]
eval (Val x) =[x | x > 0]
eval (App op exp1 exp2) = [apply op x y | x <- eval exp1, y <- eval exp2, valid op x y]





-- retourne toute les solutions a partir d'une liste donné
subs:: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) (subs xs) ++ subs xs

-- Possibilité d'insertion d'un éléments dans une liste
interleave::  a ->  [a] ->  [[a]]
interleave x [] = [[x]]
interleave x (y:ys) =  (x:y:ys) : map (y:) (interleave x ys)

-- Retourne toute les possibilité d'insertion
perms:: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat [interleave x ys | ys <- perms xs]

-- Retourne toute les construction possible 
choices:: [a] -> [[a]]
choices xs = concat [perms ys | ys <- subs xs]







solution:: Expr -> [Int] -> Int -> Bool
solution e ys x = (elem (values e) (choices ys)) && (eval e == [x])






-- Retourne toute les facon de découper une liste
split  ::  [a] ->  [([a],[a])]
split liste = [(take i liste, drop i liste) | i <- [0.. length liste -1]]

ops:: [Op]
ops = [Add, Sub, Mul, Div]

-- Génère toute les expressions possible à partir de 2 expressions
combine :: Expr -> Expr -> [Expr]
combine exp1 exp2 = [App o exp1 exp2 | o <- ops]

-- Génére toute les expression arithmétiques possible dans le contexte d'une séquence de nombre données
exprs:: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ e | (ls, rs) <-split ns,
                        l <- exprs ls,
                        r <- exprs rs,
                        e <- combine l r]


solutions  ::  [Int] ->  Int ->  [Expr]
solutions xs x = [exp1 | xs' <- choices xs, exp1 <- exprs xs', eval exp1 == [x]]
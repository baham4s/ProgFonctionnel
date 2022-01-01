import Prelude hiding (zip, unzip, takeWhile, getLine, rmdups)

data Btree a = Leaf a | Node (Btree a) a (Btree a)
data BoolExpr a = Var Char | 
                  Not (BoolExpr a) |
                  And (BoolExpr a) (BoolExpr a) |
                  Or (BoolExpr a) (BoolExpr a)

data Assoc = Ass [(Char,Bool)] deriving Show

aexpr = Or (Var 'D') (And (Var 'B') (Var 'C'))
aassoc = Ass [('D', False), ('B', True), ('C', False)]
exp1 = Not (And (Or (Var 'B') (Var 'C')) (Var 'D'))
exp2 = And (Or (Var 'A') (Var 'B')) (And (Var 'A') (Var 'D'))

postflatten :: Btree a -> [a]
postflatten (Leaf x) = [x]
postflatten (Node left x right) = postflatten left ++ [x] ++ postflatten right

infflatten :: Btree a -> [a]
infflatten (Leaf x) = [x]
infflatten (Node left x right) = infflatten left ++ infflatten right ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((a,b):xs) = (a:z, b:z)
  where z = unzip xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile z (x:xs) = if z x
                       then x : takeWhile z xs
                       else []



instance (Show a) => Show (BoolExpr a) where
    show (Var x) = show x
    show (Not x) = "Not (" ++ show x ++ ")"
    show (And x y) = "(" ++ show x ++ "." ++ show y ++ ")"
    show (Or x y) = "(" ++ show x ++ "+" ++ show y ++ ")"

getVal :: Char -> Assoc -> Bool
getVal _ (Ass []) = False
getVal c (Ass(x:xs)) = if fst x == c
                    then snd x
                    else getVal c (Ass xs)

getVar :: BoolExpr a -> [Char]
getVar (Var x) = [x]
getVar (Not x) = getVar x
getVar (And x y) = getVar x ++ getVar y
getVar (Or x y) = getVar x ++ getVar y

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

getVars :: BoolExpr a -> [Char]
getVars x = rmdups (getVar x)

eval:: Expr -> [Int]
eval (Val x) =[x | x > 0]
eval (App op exp1 exp2) = [apply op x y | x <- eval exp1, y <- eval exp2, valid op x y]


-- eval :: Assoc -> BoolExpr a -> Bool
-- eval (Ass []) _ = False
-- eval (Ass x) (Var a) = getVal a x
-- eval (Ass x) (Not a) = eval x a
-- eval (Ass x) (And a b) = (eval x a) && (eval x b)
-- eval (Ass x) (Or a b) = (eval x a) || (eval x b)

var2str :: String -> String
var2str [] = []
var2str (x:xs) = ' ' : x : ' ' : var2str xs


showHeader :: [Char] -> IO ()
showHeader [] = putStr []
showHeader x = do
                 putStrLn (var2str x ++ "| X")
                 putStrLn (replicate 13 '-')
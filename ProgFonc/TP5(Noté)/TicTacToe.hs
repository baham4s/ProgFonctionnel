-- DUBIN Baptiste
import Data.Char
import Data.List
import System.IO

-- DECLARATION BASIQUE
size :: Int
size = 3 -- On fixe ici globalement la taille de la grille

data Player = O | X | B 
  deriving (Eq, Ord)

-- Affichage d'un joueur en String
show_bis :: Player -> String
show_bis O = "O"
show_bis X = "X"
show_bis B = " "

type Grid = [[Player]] -- La grille est une liste de lignes

-- Ordre des tours de jeu
next :: Player -> Player
next X = O
next O = X
next B = B


-- FONCTION UTILITAIRE POUR LA GRILLE

-- Vérifie si la ligne ne contient que des blank
empty :: Grid
empty = replicate size(replicate size B)

-- Vérifie si la ligne ne contient aucun blank
full :: Grid -> Bool
full g = all(/= B) (concat g)

-- Permet d'obtenir la diagonal de gauche a droite de la Grid
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

-- Détermine si un joueur a 3 valeur identique soit dans la colonne soit dans une ligne soit dans une diagonal
wins :: Player -> Grid -> Bool
wins p g = any (all (==p)) (l ++ c ++ d)
  where
    l = g
    c = transpose g
    d = [diag g, diag (map reverse g)]

-- Test si la grille est gagnante pour l'un des 2 joueurs
won :: Grid -> Bool
won g = wins O g || wins X g


-- AFFICHAGE DE LA GRILLE
-- Affichage d'une barre entre chaque valeur d'une colonne
insVert :: [String] -> [String]
insVert [x] = [x]
insVert (x:xs) = (x : "|" : insVert xs)

-- Affichage en mode visuel des Player
showRow :: [Player] -> [String]
showRow [x] = [show_bis x]
showRow (x:xs) = show_bis x : "|" : showRow xs

-- Affichage de tiret entre chaque ligne de la Grid
insHoriz  ::  [String]
insHoriz = ["-" | _ <- [1..size*2]]

-- Affichage de la Grid en 2 par 2 càd ligne(g)+ligne(-)
showGrid :: Grid -> IO()
showGrid [] = putStr []
showGrid [x] = do
                 putStr (concat (showRow x))
                 putChar '\n'
showGrid (g:gs) = do
                   putStr (concat (showRow g))
                   putChar '\n'
                   putStr (concat insHoriz)
                   putChar '\n'
                   showGrid gs


-- MODIFICATION DE LA GRILLE
-- Indique si la position choisi est valide
valid :: Grid -> Int -> Bool
valid g x = 0 <= x && x < size ^ 2 && concat g !! x == B

-- Format la Grid en la coupant par size de la Grid
cut :: Int -> [a] -> [[a]]
cut n [] = []
cut n x = take n x : (cut n (drop n x))

-- Ajout de la valeur saisie
move :: Grid -> Int -> Player -> [Grid]
move g x p = if valid g x
               then [cut size (xs ++ [p] ++ ys)]
               else []
          where
            xs = take (x) (concat g)
            ys = drop (x+1) (concat g)

-- JEU HUMAIN VS HUMAIN
cls :: IO () 
cls = putStr "\ESC[2J" 
 
goto :: (Int, Int) -> IO () 
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO () 
run g p = do cls            -- clear screen 
             goto (1,1)     -- go to the upper left position 
             showGrid g 
             run' g p 
 
getNat :: String -> IO Int 
getNat message = 
    do putStr message 
       xs <- getLine 
       if xs /= [] && all isDigit xs 
          then return (read xs) 
          else do putStrLn "Error: invalid number" 
                  getNat message 
 
tictactoe :: IO () 
tictactoe = run empty O 
 
prompt :: Player -> String 
prompt p = "Player " ++ show_bis p ++ ", enter your move: "

run' :: Grid -> Player -> IO () 
run' g p | wins O g = putStrLn "Player O wins!\n" 
         | wins X g= putStrLn "Player X wins!\n" 
         | full g = putStrLn "It's a draw!\n" 
         | otherwise = 
            do i <- getNat (prompt p)
               case move g i p of 
                []   -> do putStrLn "Error: invalid move" 
                           run' g p
                [g'] -> run g' (next p)
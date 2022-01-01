import Prelude hiding (zip3, unzip)

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 [] _ _ = []
zip3 _ [] _ = []
zip3 _ _ [] = []
zip3 (x:xs)(y:ys)(z:zs) = (x,y,z) : zip3 xs ys zs

unzip :: [(a,b)] -> ([a], [b])
unzip xs = foldr f x xs
             where
               f (a,b) (as, bs) = (a:as, b:bs)
               x                = ([], [])

data Joueur = JBlanc | JNoir

data Pion = Vide | Noir | Blanc | DameN | DameB
  deriving (Eq, Ord)

type Ligne = [Pion]
type Damier = [Ligne]
type Position = (Int, Int)

size :: Int
size = 9

instance Show Pion where
    show (Vide) = show ' '
    show (Noir) = show 'N'
    show (Blanc) = show 'B'
    show (DameB) = show "DB"
    show (DameN) = show "DN"

insHoriz :: [Ligne]
insHoriz = [Vide | _ <- [1..size*5]]

line1 :: Pion -> [String]
line1 c = [" | | " ++ show c | _ <- [1..5]]

line2 :: Pion -> [String]
line2 c = [show c ++ " | | " | _ <- [1..5]]

damierDebut :: Damier 
damierDebut = [insHoriz ++ line1 Blanc ++ insHoriz ++ line2 Blanc]

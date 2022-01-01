data Parfum = Chocolat | Vanille | Framboise
data Glace = UneBoule Parfum | DeuxBoules Parfum Parfum | TroisBoules Parfum Parfum Parfum

prixParfum :: Parfum -> Float
prixParfum Chocolat = 1.5
prixParfum Vanille = 1.2
prixParfum Framboise = 1.4

prixGlace :: Glace -> Float
prixGlace (UneBoule x) = 0.1 + prixParfum x
prixGlace (DeuxBoules x y) = sum(0.15 : (map prixParfum(x:y:[])))
prixGlace (TroisBoules x y z) = sum(0.20 : (map prixParfum(x:y:z:[])))
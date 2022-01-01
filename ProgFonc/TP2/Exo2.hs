paire :: Int -> Bool
paire 0 = True
paire a = impaire (a -1)

impaire :: Int -> Bool
impaire 0 = False
impaire a = paire (a -1)

drop_bis :: Int -> [a] -> [a]
drop_bis _ [] = []
drop_bis 1 (_:xs) = xs
drop_bis a (_:xs) = drop_bis (a - 1) xs

take_bis :: Int -> [a] -> [a]
take_bis _ [] = []
take_bis 1 (x:_) = [x]
take_bis a (x : xs) = x : take_bis (a - 1) xs

halve :: [a] -> ([a], [a]) 
halve xs = ((take_bis s xs), (drop_bis s xs))
           where
             s = div (length xs) 2

merge :: [a] -> [a] -> [a]
merge [] _ = []
merge _ [] = []
merge (x : xs) (y : ys) = x : y : merge xs ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort rs) (msort ls)
    where (rs, ls) = halve xs
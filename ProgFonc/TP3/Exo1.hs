type Matf = Int -> Int -> (Bool, Int)

exemple :: Matf
exemple i j = if (i >= 1) && (i <= 6) && (j >= 1) && (i <= 5)
                then (True, 2*i+j)
                else (False, 0)

identite4x4 :: Matf
identite4x4 i j = if (i >= 1) && (i <= 4) && (j >= 1) && (i <= 4) && i == j
                    then (True, 1)
                    else if (i >= 1) && (i <= 4) && (j >= 1) && (i <= 4) && i == j
                        then (True, 0)
                        else (False, 0)

-- nbLinesIntern :: Matf -> Int -> Int
-- nbLinesIntern f a = case f a 1 of (True, _) -> 1 + nbLinesIntern f a + 1
--                                   (False, _) -> 0

-- nbLines :: Matf -> Int
-- nbLines f = nbLinesIntern f 1

-- nbColsIntern :: Matf -> Int -> Int
-- nbColsIntern f a = case f a 1 of (True, _) -> 1 + nbColsIntern f a + 1
--                                  (False, _) -> 0

-- nbCols :: Matf -> Int
-- nbCols f = nbColsIntern f 1

-- Correction 

nbLines :: Matf -> Int
nbLines f = nbLinesIntern f 1
    where nbLinesIntern f i = case f i 1 of (True, _) -> 1 + nbLinesIntern f i+1
                                            (False, _) -> 0

nbCols :: Matf -> Int
nbCols f = nbColsInterne f 1
    where nbColsInterne f j = case f j 1 of (True, _) -> 1 + nbColsInterne f j+1
                                            (False, _) -> 0

dims :: Matf -> (Int, Int)
dims m = (nbLines m, nbCols m)

cmpDims :: Matf -> Matf -> Bool
cmpDims a b = if (dims a /= dims b)
                then False
                else True

add :: Matf -> Matf -> Matf
add a b = if not (cmpDims a b)
            then error "problème"
            else let (l, c) = dims a in
                 (\i j -> if 1 <= i && i <= l && 1 <= j && j <= c
                     then (True, snd (a i j) + snd (b i j))
                     else (False, 0))

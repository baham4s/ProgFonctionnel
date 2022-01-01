sumcarre :: Int
sumcarre = sum[x * x | x <- [1..100]]

replic :: Int -> b -> [b]
replic a b = [ b | _ <- [1..a]]

pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x, y, z) | x <- [1..a], y <- [1..a], z <- [1..a], x^2+y^2 == z^2]
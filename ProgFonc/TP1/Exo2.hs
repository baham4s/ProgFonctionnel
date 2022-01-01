fonc :: Int -> Int -> Int -> Int -> Bool
fonc a b c d = if (a == b) && (b == c) && (c == d)
                 then True
                 else False

foncb :: Int -> Int -> Int -> Int -> Int
foncb a b c d = if (a > b) && (a > c) && (a > d)
                  then a
                  else if (b > c) && (b > d)
                      then b
                      else if (c > d)
                          then c
                          else d
                          
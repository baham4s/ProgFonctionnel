type Domino = (Int, Int)

domino :: Domino -> Domino -> Bool
domino (a,b) (c,d) = if (a /= c && a /= d && b /= c && b /= d)
                       then False
                       else True

dominobis :: Domino -> Domino -> Domino -> Bool
dominobis (a,b) (c,d) (e,f) = if (a /= c && a /= d && b /= c && b /= d)
                                then False
                                else if (c /= e && c /= f && d /= e && d /= f)
                                  then False
                                  else True

